;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/apropos.lisp,v 1.32 2015/03/06 12:54:25 edi Exp $

;;; Copyright (c) 2005-2015, Dr. Edmund Weitz.  All rights reserved. 

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :lw-add-ons)

(defclass apropos-result-panel (capi:multi-column-list-panel)
  ()
  (:documentation "A subclass of CAPI:MULTI-COLUMN-LIST-PANEL which
solely exists for the purpose of specializing
CAPI:MAKE-PANE-POPUP-MENU - see below."))

(capi:define-interface apropos-dialog (lispworks-tools::lispworks-interface)
  ((search-list :initform nil
                :accessor apropos-dialog-search-list
                :documentation "A list of previous search strings.")
   (search-string :initform nil
                  :accessor apropos-dialog-search-string
                  :documentation "The current search string.")
   (result-list :initform nil
                :accessor apropos-dialog-result-list
                :documentation "A list of symbols that are the result
of the current Apropos search.")
   (sort-key :initform :name
             :accessor apropos-dialog-sort-key
             :documentation "A keyword denoting how the result panel
is currently sorted.  Each keyword corresponds to a column header.")
   (type-test :initform (constantly t)
              :accessor apropos-dialog-type-test
              :documentation "The function that's currently used to
check whether a symbol should be included in the result panel.
Controlled by the buttons within the `Show' frame."))
  (:panes
   (string-input
    capi:text-input-choice
    :accessor apropos-dialog-string-input
    :title "String: "
    :text (get-apropos-user-preference "string-input" "")
    :callback-type :interface
    :callback 'update-search-list
    :selection-callback 'update-result-list
    :items nil)
   (search-button
    capi:push-button
    :text "Search"
    :callback-type :interface
    :callback 'update-search-list)
   (exported-button
    capi:check-button
    :accessor apropos-dialog-exported-button
    :selected (get-apropos-user-preference "exported-button" t)
    :callback-type :interface
    :selection-callback 'update-result-panel
    :retract-callback 'update-result-panel
    :text "Show only exported symbols")
   (present-symbols-button
    capi:check-button
    :accessor apropos-dialog-present-symbols-button
    :selected (get-apropos-user-preference "present-symbols-button" nil)
    :enabled (not (get-apropos-user-preference "all-packages-button" t))
    :callback-type :interface
    :selection-callback 'update-result-panel
    :retract-callback 'update-result-panel
    :text "Show only symbols present in selected package")
   (warn-on-long-searches-button
    capi:check-button
    :accessor apropos-dialog-warn-on-long-searches-button
    :selected (get-apropos-user-preference "warn-on-long-searches-button" t)
    :callback-type :none
    :text "Warn on \(most) long searches")
   (regex-button
    capi:check-button
    :accessor apropos-dialog-regex-button
    :selected (get-apropos-user-preference "regex-button" t)
    :callback-type :interface
    :selection-callback 'update-result-list
    :retract-callback 'update-result-list
    :text "Search string is regular expression")
   (all-packages-button
    capi:check-button
    :accessor apropos-dialog-all-packages-button
    :text "All"
    :callback-type :none
    :selected (get-apropos-user-preference "all-packages-button" t)
    ;; disable some stuff when this button is checked
    :selection-callback (lambda ()
                          (setf (capi:option-pane-enabled package-pull-down) nil
                                (capi:button-enabled present-symbols-button) nil)
                          (update-result-list capi:interface))
    :retract-callback (lambda ()
                        (setf (capi:option-pane-enabled package-pull-down) t
                              (capi:button-enabled present-symbols-button) t)
                        (update-result-list capi:interface)))
   (package-pull-down
    capi:option-pane
    :accessor apropos-dialog-package-pull-down
    :items (sort (list-all-packages) #'string< :key #'package-name)
    :print-function #'package-name
    :enabled (not (get-apropos-user-preference "all-packages-button" t))
    :selected-item (let ((package-name (get-apropos-user-preference "package-pull-down" "LW-ADD-ONS")))
                     (or (and package-name
                              (find-package package-name))
                         (find-package :lw-add-ons)))
    :callback-type :interface
    :selection-callback 'update-result-list)
   (all-types-button
    capi:check-button
    :accessor apropos-dialog-all-types-button
    :text "All"
    :selected (get-apropos-user-preference "all-types-button" t)
    :callback-type :none
    ;; disable the other three buttons when this button is checked
    :selection-callback (lambda ()
                          (setf (capi:button-enabled variables-button) nil
                                (capi:button-enabled functions-button) nil
                                (capi:button-enabled classes-button) nil)
                          (update-type-test capi:interface))
    :retract-callback (lambda ()
                        (setf (capi:button-enabled variables-button) t
                              (capi:button-enabled functions-button) t
                              (capi:button-enabled classes-button) t)
                        (update-type-test capi:interface)))
   (variables-button
    capi:check-button
    :accessor apropos-dialog-variables-button
    :text "Variables"
    :enabled (not (get-apropos-user-preference "all-types-button" t))
    :selected (get-apropos-user-preference "variables-button" nil)
    :callback-type :interface
    :selection-callback 'update-type-test
    :retract-callback 'update-type-test)
   (functions-button
    capi:check-button
    :accessor apropos-dialog-functions-button
    :text "Functions"
    :enabled (not (get-apropos-user-preference "all-types-button" t))
    :selected (get-apropos-user-preference "functions-button" t)
    :callback-type :interface
    :selection-callback 'update-type-test
    :retract-callback 'update-type-test)
   (classes-button
    capi:check-button
    :accessor apropos-dialog-classes-button
    :text "Classes"
    :enabled (not (get-apropos-user-preference "all-types-button" t))
    :selected (get-apropos-user-preference "classes-button" nil)
    :callback-type :interface
    :selection-callback 'update-type-test
    :retract-callback 'update-type-test)
   (result-panel
    apropos-result-panel
    :accessor apropos-dialog-result-panel
    :title ""
    :interaction :extended-selection
    #-:no-right-click-selection-behavior #-:no-right-click-selection-behavior
    :right-click-selection-behavior :temporary-selection
    :callback-type :item-interface
    :action-callback (lambda (item interface)
                       (let* ((symbol-name (first item))
                              (symbol-package (second item))
                              (symbol (intern symbol-name (find-package symbol-package))))
                         (cond ((source-can-be-found symbol)
                                (ignore-errors*
                                  (lispworks-tools::interface-find-source interface symbol)))
                               ((documentation-uri symbol)
                                (browse-anchored-uri (documentation-uri symbol))))))
    :columns '((:width (:character 60))
               (:width (:character 40))
               (:width (:character 10))
               (:width (:character 10))
               (:adjust :center :width (:character 10))
               (:adjust :center :width (:character 10)))
    :header-args `(:items ,+apropos-headline+
                   :alignments (:left :left :center :center :center :center)
                   :callback-type :item
                   ;; clicking on a column header changes the sort order
                   :selection-callback ,(lambda (item)
                                          (setq sort-key
                                                (case (position item +apropos-headline+ :test #'string=)
                                                  (1 :package)
                                                  (2 :fun)
                                                  (3 :var)
                                                  (4 :class)
                                                  (5 :exp)
                                                  (otherwise :name)))
                                          (re-sort-result-panel capi:interface)))
    :sort-descriptions (mapcar (lambda (type key)
                                 (capi:make-sorting-description
                                  :type type
                                  :key key
                                  :sort #'string-lessp
                                  :reverse-sort #'string-greaterp))
                               '(:name :package :fun :var :class :exp)
                               (list #'first #'second #'third #'fourth #'fifth #'sixth))))
  (:layouts
   (string-layout
    capi:row-layout
    '(string-input search-button))
   (button-layout
    capi:grid-layout
    '(exported-button present-symbols-button regex-button warn-on-long-searches-button)
    :columns 2
    :x-gap 5
    :y-gap 5)
   (package-layout
    capi:row-layout
    '(all-packages-button package-pull-down)
    :adjust :center
    :title "Package(s) to search"
    :title-position :frame)
   (type-layout
    capi:grid-layout
    '(all-types-button variables-button functions-button classes-button)
    :columns 2
    :x-gap 5
    :y-gap 5
    :title "Show"
    :title-position :frame)
   (left-control-layout
    capi:column-layout
    '(button-layout package-layout))
   (control-layout
    capi:row-layout
    '(left-control-layout type-layout))
   (main-layout
    capi:column-layout
    '(string-layout control-layout result-panel)))
  (:default-initargs
   :layout 'main-layout
   :create-callback (lambda (interface)
                      ;; we have to jump through some hoops -
                      ;; see <http://thread.gmane.org/gmane.lisp.lispworks.general/4873>
                      (mp:process-run-function
                       "apropos-preselect-text" nil
                       (lambda ()
                         (capi:execute-with-interface
                          interface
                          (lambda (interface)
                            (let* ((string-input (apropos-dialog-string-input interface))
                                   (text (capi:text-input-pane-text string-input)))
                              (capi:set-pane-focus string-input)
                              (capi:set-text-input-pane-selection string-input
                                                                  0
                                                                  (length text))))
                          interface))))
   :destroy-callback (lambda (interface)
                       (set-apropos-user-preferences
                        "exported-button"
                        (capi:button-selected
                         (apropos-dialog-exported-button interface))
                        "present-symbols-button"
                        (capi:button-selected
                         (apropos-dialog-present-symbols-button interface))
                        "warn-on-long-searches-button"
                        (capi:button-selected
                         (apropos-dialog-warn-on-long-searches-button interface))
                        "regex-button"
                        (capi:button-selected
                         (apropos-dialog-regex-button interface))
                        "all-packages-button"
                        (capi:button-selected
                         (apropos-dialog-all-packages-button interface))
                        "all-types-button"
                        (capi:button-selected
                         (apropos-dialog-all-types-button interface))
                        "variables-button"
                        (capi:button-selected
                         (apropos-dialog-variables-button interface))
                        "functions-button"
                        (capi:button-selected
                         (apropos-dialog-functions-button interface))
                        "classes-button"
                        (capi:button-selected
                         (apropos-dialog-classes-button interface))
                        "string-input"
                        (capi:text-input-pane-text
                         (apropos-dialog-string-input interface))
                        "package-pull-down"
                        (package-name
                         (capi:choice-selected-item
                          (apropos-dialog-package-pull-down interface))))))
  (:documentation "The definition of the CAPI interface that's used to
display the Apropos Dialog."))
  
(defmethod update-type-test ((interface apropos-dialog))
  "Updates the TYPE-TEST slot of INTERFACE according to the buttons in
the `Show' frame.  Calls UPDATE-RESULT-PANEL afterwards."
  (with-accessors ((type-test apropos-dialog-type-test)
                   (all-types-button apropos-dialog-all-types-button)
                   (variables-button apropos-dialog-variables-button)
                   (functions-button apropos-dialog-functions-button)
                   (classes-button apropos-dialog-classes-button))
      interface
    (setq type-test
          (cond ((capi:button-selected all-types-button)
                 (constantly t))
                (t (let ((variablesp (capi:button-selected variables-button))
                         (functionsp (capi:button-selected functions-button))
                         (classesp (capi:button-selected classes-button)))
                     (cond ((or variablesp functionsp classesp)
                            (lambda (symbol)
                              (or (and variablesp
                                       (boundp symbol))
                                  (and functionsp
                                       (fboundp symbol))
                                  (and classesp
                                       (find-class symbol nil)))))
                           (t (constantly nil)))))))
    (update-result-panel interface)))

(defmethod update-search-list ((interface apropos-dialog))
  "Updates the SEARCH-STRING slot of INTERFACE from the input provided
by the STRING-INPUT pane.  SEARCH-LIST is also modified accordingly
and afterwards UPDATE-RESULT-LIST is called."
  (with-accessors ((search-list apropos-dialog-search-list)
                   (search-string apropos-dialog-search-string)
                   (string-input apropos-dialog-string-input)
                   (warn-on-long-searches-button apropos-dialog-warn-on-long-searches-button)
                   (all-packages-button apropos-dialog-all-packages-button))
      interface
    (let ((string (capi:text-input-pane-text string-input))
          cancelp)
      (when (and (< (length string) 3)
                 (cond ((capi:button-selected all-packages-button) t)
                       (t (capi:button-selected warn-on-long-searches-button))))
        (setq cancelp
              (not (capi:confirm-yes-or-no "Search string is very short, APROPOS might take a looooong time.~%Do you really want to start the search?"))))
      (unless cancelp
        (pushnew string search-list :test #'string=)
        (when (> (length search-list) *apropos-max-search-list-length*)
          (setq search-list (subseq search-list 0 *apropos-max-search-list-length*)))
        (setf (capi:collection-items string-input)
              (sort (copy-list search-list) #'string-lessp)
              (capi:text-input-pane-text string-input)
              string
              search-string
              string)
        (update-result-list interface)))))

(defmethod update-result-list ((interface apropos-dialog))
  "Updates the RESULT-LIST slot of INTERFACE according to
SEARCH-STRING, the REGEX-BUTTON button and the package selection.
Calls UPDATE-RESULT-PANEL afterwards."
  (with-accessors ((result-list apropos-dialog-result-list)
                   (search-string apropos-dialog-search-string)
                   (regex-button apropos-dialog-regex-button)
                   (all-packages-button apropos-dialog-all-packages-button)
                   (package-pull-down apropos-dialog-package-pull-down))
      interface
    (when search-string
      (let ((regex (cond ((capi:button-selected regex-button)
                          search-string)
                         (t (quote-meta-chars search-string))))
            (package (and (not (capi:button-selected all-packages-button))
                          (capi:choice-selected-item package-pull-down))))
        (setq result-list
              (handler-case 
                  (sort (remove-duplicates (regex-apropos-list regex package)
                                           :test #'eq)
                        #'string-lessp :key #'symbol-name)
                (error (msg)
                       (capi:display-message "~A" msg)
                       nil)))))
    (update-result-panel interface)))

(defun symbol-exported-p (symbol)
  "Returns a true value iff the symbol SYMBOL is exported from its
home package."
  (eq (nth-value 1 (find-symbol (symbol-name symbol)
                                (symbol-package symbol)))
      :external))

(defun function-info (symbol)
  "Returns a string with information about the symbol SYMBOL that can
be used for the `Fun' column of the result panel."
  (cond ((special-operator-p symbol) "special op")
        ((macro-function symbol) "macro")
        ((fboundp symbol)
         (cond ((typep (symbol-function symbol) 'generic-function) "generic")
               (t "function")))
        (t "")))

(defun var-info (symbol)
  "Returns a string with information about the symbol SYMBOL that can
be used for the `Var' column of the result panel."
  (cond ((constantp symbol) "const")
        ((boundp symbol) "bound")
        (t "")))

(defmethod update-result-panel ((interface apropos-dialog))
  "Updates the items shown in the result panel of INTERFACE according
to the contents of the RESULT-LIST slot and various other settings."
  (with-accessors ((result-list apropos-dialog-result-list)
                   (result-panel apropos-dialog-result-panel)
                   (search-string apropos-dialog-search-string)
                   (regex-button apropos-dialog-regex-button)
                   (exported-button apropos-dialog-exported-button)
                   (present-symbols-button apropos-dialog-present-symbols-button)
                   (package-pull-down apropos-dialog-package-pull-down)
                   (type-test apropos-dialog-type-test))
      interface
    (when search-string
      (let* ((selected-package (capi:choice-selected-item package-pull-down))
             (show-present-symbols-p (and selected-package
                                          (capi:button-enabled present-symbols-button)
                                          (capi:button-selected present-symbols-button)))
             (package-test (cond ((and show-present-symbols-p
                                       (capi:button-selected exported-button))
                                  (lambda (symbol)
                                    (and (symbol-exported-p symbol)
                                         (eq (symbol-package symbol) selected-package))))
                                 (show-present-symbols-p
                                  (lambda (symbol)
                                    (eq (symbol-package symbol) selected-package)))
                                 ((capi:button-selected exported-button)
                                  #'symbol-exported-p)
                                 (t (constantly t)))))
        (setf (capi:titled-object-title result-panel)
              (format nil "Symbols ~:[containing~;matching~] ~S"
                      (capi:button-selected regex-button)
                      search-string)
              (capi:collection-items result-panel)
              (loop for symbol in result-list
                    when (and (funcall package-test symbol)
                              (funcall type-test symbol))
                    collect (list (symbol-name symbol)
                                  (package-name (symbol-package symbol))
                                  (function-info symbol)
                                  (var-info symbol)
                                  (if (find-class symbol nil) "x" "")
                                  (if (symbol-exported-p symbol) "x" ""))))))))

(defmethod re-sort-result-panel ((interface apropos-dialog))
  "Changes the sort order of the items in the result panel of
INTERFACE according to the SORT-KEY slot."
  (with-accessors ((result-panel apropos-dialog-result-panel)
                   (search-string apropos-dialog-search-string)
                   (sort-key apropos-dialog-sort-key))
      interface
    (when search-string
      (capi:sorted-object-sort-by result-panel sort-key))))

(defmethod capi:make-pane-popup-menu ((result-panel apropos-result-panel)
                                      (interface apropos-dialog)
                                      &key &allow-other-keys)
  "This method is responsible for the right-click popup menu in the
Apropos dialog.  Unfortunately, this doesn't seem to work on LWM."
  (let* ((items (capi:collection-items result-panel))
         (selection (capi:choice-selection result-panel))
         (length (length selection)))
    (cond ((zerop length)
           #-:macosx nil
           ;; dummy menu to work around a deficiency in OS X
           #+:macosx (make-instance 'capi:menu
                                    :title "Dummy Menu"
                                    :items (list (make-instance 'capi:menu-item
                                                                :title "Dummy Item"))))
          ((= length 1)
           (let* ((index (first selection))
                  (item (elt items index)))
             (destructuring-bind (title menu-items)
                 (create-apropos-popup-menu item interface)
               (make-instance 'capi:menu
                              ;; add title as a dummy menu entry
                              :items (cons (make-instance 'capi:menu-component
                                                          :items (list (make-instance 'capi:menu-item
                                                                                      :title title)))
                                           menu-items)))))
          (t
           ;; if there's more than one item in the selection
           ;; each one gets its own submenu
           (make-instance 'capi:menu
                          :items (loop for index in selection
                                       for item = (elt items index)
                                       for (title menu-items) = (create-apropos-popup-menu item interface)
                                       collect (make-instance 'capi:menu
                                                              :title title
                                                              :items menu-items)))))))
      
(defun create-apropos-popup-menu (item interface)
  "Returns a list of two elements - a title and a popup menu for the
item ITEM which is a list of strings used for the result panel."
  (let* ((symbol-name (first item))
         (symbol-package (second item))
         (symbol (intern symbol-name (find-package symbol-package)))
         menu-items
         submenu-items)
    (flet ((inspect-symbol ()
             "A function that opens an IDE inspector for the symbol SYMBOL."
             (gui-inspect symbol)))
      (when-let (class (find-class symbol nil))
        (push (make-instance 'capi:menu-item
                             :callback-type :none
                             :callback (lambda ()
                                         "A function that opens a
class browser for the class named by the symbol SYMBOL."
                                         (capi:find-interface 'lispworks-tools:class-browser
                                                              :object class))
                             :title (format nil "Class: ~A" (format-object-for-apropos class)))
            submenu-items))
      (when (fboundp symbol)
        (let ((symbol-function (symbol-function symbol)))
          (push (make-instance 'capi:menu-item
                               :callback-type :none
                               :callback (cond ((typep symbol-function 'generic-function)
                                                (lambda ()
                                                  "A function that opens a generic function browser for the generic function named by the symbol SYMBOL."
                                                  (capi:find-interface 'lispworks-tools:generic-function-browser
                                                                       :object symbol-function)))
                                               (t #'inspect-symbol))
                               :title (format nil "Function cell: ~A" (format-object-for-apropos symbol-function)))
                submenu-items)))
      (when (boundp symbol)
        (push (make-instance 'capi:menu-item
                             :callback-type :none
                             :callback #'inspect-symbol
                             :title (format nil "Value cell: ~A" (format-object-for-apropos (symbol-value symbol))))
              submenu-items)))
    (when submenu-items
      (push (make-instance 'capi:menu-component :items submenu-items) menu-items)
      (setq submenu-items nil))
    (let ((uri (documentation-uri symbol)))
      (push (make-instance 'capi:menu-item
                           :enabled-function (constantly uri)
                           :title "Documentation"
                           :callback-type :none
                           ;; only enable if a documentation URI was found
                           :callback (lambda ()
                                       (browse-anchored-uri uri)))
            submenu-items))
    (push (make-instance 'capi:menu-item
                         :title "Find Source"
                         ;; only enable if we can locate the source code
                         :enabled-function (constantly
                                            (source-can-be-found symbol))
                         :callback-type :none
                         :callback (lambda ()
                                     (ignore-errors*
                                       (lispworks-tools::interface-find-source interface symbol))))
          submenu-items)
    (push (make-instance 'capi:menu-component :items submenu-items) menu-items)
    (list (format-object-for-apropos symbol) menu-items)))
