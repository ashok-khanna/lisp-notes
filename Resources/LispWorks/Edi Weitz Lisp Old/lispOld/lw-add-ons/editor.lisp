;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/editor.lisp,v 1.47 2015/03/06 12:54:25 edi Exp $

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

(defun symbol-string-at-point (&key (point (current-point)) previous)
  "Returns two values - a string denoting the symbol at POINT and
the package to use at that POINT if the symbol isn't
package-qualified.  PREVIOUS controls whether to look at the
previous symbol if POINT is between two symbols."
  (ignore-errors*
    (let ((string (editor::read-symbol-from-point :point point
                                                  :read-package-name t
                                                  :previous previous))
          (package (editor::buffer-package-to-use point)))
      (values string package))))

(defun symbol-at-point (&key (point (current-point)) previous)
  "Returns the symbol at POINT.  PREVIOUS controls whether to
look at the previous symbol if POINT is between two symbols."
  (ignore-errors*
    (multiple-value-bind (string package)
        (symbol-string-at-point :point point :previous previous)
      (let* ((*package* package)
             (candidate (read-from-string string)))
        (and (symbolp candidate)
             candidate)))))

(defun enclosing-operators ()
  "Returns a list of potential operators \(symbols behind an
opening parenthesis) starting from point and going up backwards."
  (save-excursion
    (loop while (ignore-errors*
                  (backward-up-list-command 1) t)
          when (when (looking-at "\(")
                 (forward-character-command 1)
                 (prog1
                   (symbol-at-point)
                   (forward-character-command -1)))
          collect it)))

(defun show-info (info &key full-length-p)
  "Shows the string INFO in the echo area.  Shows no more than
*MAX-INFO-LENGTH* unless FULL-LENGHT-P is true."
  (apply #'message
         (if (and (not full-length-p)
                  (> (length info) *max-info-length*))
           (list "~A [...]" (subseq info 0 *max-info-length*))
           (list "~A" info))))

(defun show-arglist ()
  "Shows the argument list of the nearest enclosing operator that
has a function definition in the echo arrea.  Shows the doc
string as well unless *SHOW-DOC-STRING-WHEN-SHOWING-ARGLIST* is
NIL."
  (when-let (object
             (loop for operator in (enclosing-operators)
                   when (and (symbolp operator)
                             (fboundp operator))
                   do (return operator)))
    (show-info (format nil "~A~@[~%~A~]"
                       (cons object (function-lambda-list object))
                       (and *show-doc-string-when-showing-arglist*
                            (documentation object 'function))))))

(defun completions-for-echo-area (completions)
  "Returns a string which shows a two-column list of the elements
\(which should be strings) in COMPLETIONS but no more than
*MAX-COMPLETIONS-TO-SHOW* of them."
  (let ((max-left-width
         (loop for completion in completions
               for i from 1 to *max-completions-to-show*
               when (oddp i)
               maximize (length completion))))
    (with-output-to-string (out)
      (format out "~&Possible completions:~%~%")
      (loop for (completion-1 completion-2 . rest) on completions by #'cddr
            for i from 1 to *max-completions-to-show* by 2
            do (format out "~&~VA  ~A" max-left-width
                       completion-1
                       (cond ((and rest
                                   (>= (1+ i) *max-completions-to-show*))
                              "[...]")
                             (completion-2)
                             (t "")))))))

(defun char-before ()
  "Returns the character before the current point."
  (character-at (current-point) -1))

(defun maybe-insert-right-parenthesis ()
  "If the symbol at or before point is in function position and
denotes a function with an empty lambda list inserts a right
parenthesis, otherwise inserts a space and show the argument list
in the echo area."
  (when-let (symbol (symbol-at-point :previous t))
    (when (and (save-excursion
                 (backward-form-command 1)
                 (eql (char-before) #\())
               (symbolp symbol)
               (fboundp symbol))
      (cond ((null (function-lambda-list symbol))
             (self-insert-command 1 #\)))
            ((not (looking-at " "))
             (insert-space-and-show-arglist-command nil))))))

#-:editor-has-dont-undo
(defmacro without-undo-with-cleanups (buffer form &body cleanups)
  "Editor utility macro.  See source code for LW editor."
  (lw:rebinding (buffer)
    (lw:with-unique-names (was-recording)
      `(let ((,was-recording (editor::check-set-buffer-without-undo ,buffer)))
         (unwind-protect
             ,form
           (when ,was-recording
             (editor::set-buffer-flag-bit ,buffer editor::*buffer-flag-dont-record-undo* nil))
           ,@cleanups)))))

#-:editor-has-dont-undo
(defmacro recording-for-undo-internal (point1 point2 line-start-p &body body)
  "Does the whole work for RECORDING-FOR-UNDO.  See source code for LW editor."
  (lw:with-unique-names (old-string start end want-undo before-modified buffer-sym)
    (lw:rebinding (point1 point2)
      `(let* ((,buffer-sym (point-buffer ,point1))
              (,want-undo (editor::check-want-to-record-undo-p ,buffer-sym nil))
              (,before-modified (editor::buffer-modified-tick ,buffer-sym))
              (,start (when ,want-undo 
                         (let ((lsp ,line-start-p)
                               (sp (copy-i-point ,point1 :before-insert)))
                           (if lsp 
                             (line-start sp))
                           sp)))
              (,end (when ,want-undo (copy-i-point ,point2 :after-insert)))
              (,old-string (when ,want-undo
                             (editor::points-to-buffer-string ,start ,end))))
         (without-undo-with-cleanups ,buffer-sym
             (progn ,@body)
           (when ,want-undo
             (editor::record-replace-region ,start ,end ,old-string ,before-modified)
             (editor::delete-it ,start)
             (editor::delete-it ,end)))))))

#-:editor-has-dont-undo
(defmacro recording-for-undo (point1 point2 &body body)
  "Performs code in BODY and records changes between POINT1 and
POINT2 for undo operation.  See source code for LW editor."
  `(recording-for-undo-internal ,point1 ,point2 nil ,@body))

#-:editor-has-dont-undo
(defmacro recording-for-undo-locking (point1 point2 &body body)
  "Like RECORDING-FOR-UNDO, but with lock.  See source code for LW editor."
  (lw:rebinding (point1)
    #-:editor-does-not-have-with-buffer-locked
    `(with-buffer-locked ((point-buffer ,point1))
       (recording-for-undo ,point1 ,point2 ,@body))
    #+:editor-does-not-have-with-buffer-locked
    `(editor::with-locked-buffer (point-buffer ,point1)
       (recording-for-undo ,point1 ,point2 ,@body))))

#+:editor-has-dont-undo
(defmacro recording-for-undo (point1 point2 &body body)
  "Performs code in BODY and records changes between POINT1 and
POINT2 for undo operation.  See source code for LW editor."
  (lw:with-unique-names (old-string start end dont changed)
    (lw:rebinding (point1 point2)
      `(let* ((,dont editor::*dont-undo*)
              (,changed (buffer-modified (point-buffer ,point1)))
              (,start (unless ,dont (copy-i-point ,point1 :before-insert)))
              (,end (unless ,dont (copy-i-point ,point2 :after-insert)))
              (,old-string (unless ,dont (editor::points-to-buffer-string ,point1 ,point2))))
         (unwind-protect
             (let ((editor::*dont-undo* t))
               ,@body)
           (progn
             (unless ,dont
               (editor::record-replace-region ,start ,end ,old-string ,changed)
               (editor::delete-it ,start)
               (editor::delete-it ,end))))))))

(defmacro recording-for-undo% (point1 point2 &body body)
  "Helper macro which dispatches to RECORDING-FOR-UNDO or
RECORDING-FOR-UNDO-LOCKING depending on the LispWorks release."
  #-:editor-has-dont-undo
  `(recording-for-undo-locking ,point1 ,point2
     ,@body)
  #+:editor-has-dont-undo
  `(recording-for-undo ,point1 ,point2
     ,@body))

(defun current-line ()
  "Returns the line the point is currently on as a string."
  (line-string (current-point)))

(defun can-move-upwards-p ()
  "Returns true if it is possible to move backward up from the
current point."
  (save-excursion
    (with-point ((point (current-point)))
      (backward-up-list-command 1)
      (point< (current-point) point))))

(defadvice (editor:find-alternate-file-command change-prompt :around
                                               :documentation "Makes
sure FIND-ALTERNATE-FILE-COMMAND provides the full pathname of the
current buffer as the default when prompting.")
    (p &optional pathname (buffer (current-buffer)))
  (let ((*change-default-for-file-prompt* t))
    (call-next-advice p pathname buffer)))

(defadvice (editor:find-alternate-file-command refresh :after
                                               :documentation "After
FIND-ALTERNATE-COMMAND has run makes sure the contents of the buffer
are consistent with the file on disk.")
    (p &optional pathname (buffer (current-buffer)))
  (declare (ignore p pathname))
  (let ((pathname (buffer-pathname buffer)))
    (unless (check-disk-version-consistent pathname buffer)
      (let* ((tn (probe-file pathname))
             (pn (or tn (editor::canonical-pathname pathname))))
        (editor::read-da-file pn tn buffer)))))

(defadvice (editor:prompt-for-file change-prompt :around
                                   :documentation "When DEFAULT-STRING
is NIL, DEFAULT is a pathname, and *CHANGE-DEFAULT-FOR-FILE-PROMPT* is
true sets the full namestring of DEFAULT to be the default string.")
    (&rest rest &key default default-string &allow-other-keys)
  (let ((default-string (cond (default-string)
                              ((and *change-default-for-file-prompt*
                                    (pathnamep default))
                               (namestring default))
                              ((pathnamep default)
                               (namestring (pathname-location default)))
                              (t default))))
    (apply #'call-next-advice
           :default-string default-string
           rest)))
                                        
(defadvice (editor::find-pattern region-only :around
                                 :documentation "Searches only up
until *SEARCH-END* unless the value of this variable is NIL.")
    (point pattern &optional limit)
  (cond ((and (null limit)
              *search-end*)
         (call-next-advice point pattern *search-end*))
        (t (call-next-advice point pattern limit))))

#-:editor-does-not-have-i-find-pattern
(defadvice (editor::i-find-pattern region-only :around
                                   :documentation "Searches only up
until *SEARCH-END* unless the value of this variable is NIL.")
    (point pattern &optional limit)
  (cond ((and (null limit)
              *search-end*)
         (call-next-advice point pattern *search-end*))
        (t (call-next-advice point pattern limit))))

(defadvice (editor::query-replace-string region-only :around
                                         :documentation "Performs
operation only up until *SEARCH-END* unless the value of this variable
is NIL.  Also makes sure that all replacements can be undone with one
undo command.")
    (&rest rest &key (point (current-point)) &allow-other-keys)
  (let* ((current-mark (and (variable-value-if-bound 'editor::active-region-overlay
                                                     :buffer (current-buffer))
                            (current-mark nil t)))
         (switch-p (and current-mark
                        (point< current-mark (current-point))))
         (*search-end* (and current-mark
                            (copy-point (cond (switch-p (current-point))
                                              (t current-mark)))))
         (start (cond ((and current-mark switch-p)
                       current-mark)
                      (current-mark (current-point))
                      (t point))))
    (unwind-protect
        (with-point ((%start start)
                     (%end (or *search-end*
                               (current-point))))
          (unless *search-end*
            (editor:buffer-end %end))
          #+:editor-has-dont-undo
          (recording-for-undo %start %end
            (apply #'call-next-advice :point start rest))
          ;; in new LispWorks versions it is no longer necessary to
          ;; record for undo here
          #-:editor-has-dont-undo          
          (apply #'call-next-advice :point start rest))
      (when *search-end*
        (delete-point *search-end*)))))

(defadvice (editor::find-next-ordinary-window allow-listener :around
                                              :documentation "Allows
the \"Next Ordinary Window\" command to switch to a listener window.")
    (current-window)
  (let ((*forbidden-buffers* (remove :listener *forbidden-buffers*)))
    (call-next-advice current-window)))

#+:editor-does-not-have-go-back
(defun push-onto-definitions-stack ()
  "Pushes current point onto *FIND-DEFINITIONS-STACK* unless the
buffer isn't selectable."
  (unless (editor::forbidden-buffer-p (current-buffer))
    (push (copy-point (current-point))
          *find-definitions-stack*)))

#+:editor-does-not-have-go-back
(defadvice (find-source-command push-onto-definitions-stack :around
                                :documentation "Pushes current point
onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (find-source-for-dspec-command push-onto-definitions-stack :around
                                          :documentation "Pushes
current point onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (find-command-definition-command push-onto-definitions-stack :around
                                            :documentation "Pushes
current point onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (editor::edit-callers-command push-onto-definitions-stack :around
                                         :documentation "Pushes
current point onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (editor::edit-callees-command push-onto-definitions-stack :around
                                         :documentation "Pushes
current point onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (find-tag-command push-onto-definitions-stack :around
                             :documentation "Pushes current point onto
*FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (tags-search-command push-onto-definitions-stack :around
                                :documentation "Pushes current point
onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (push-onto-definitions-stack)
  (apply #'call-next-advice args))

#+:editor-does-not-have-go-back
(defadvice (continue-tags-search-command push-onto-definitions-stack :around
                                         :documentation "Pushes
current point onto *FIND-DEFINITIONS-STACK*.")
    (&rest args)
  (when editor::*meta-comma-action*
    (push-onto-definitions-stack))
  (apply #'call-next-advice args))

(defun complete-system (string parse-inf)
  "Completion function used by PROMPT-FOR-ASDF-SYSTEM."
  (declare (ignore parse-inf))
  (editor::complete-string string *all-asdf-systems*
                           :ignore-case t))

(defun prompt-for-asdf-system (string &optional prompt help no-check)
  "Prompts for an ASDF system name with STRING being the default."
  (let ((*all-asdf-systems* (list-asdf-systems)))
    (editor::parse-for-something
     :prompt (or prompt "ASDF system: ")
     :must-exist t
     :help (or help "Type a name of an ASDF system.")
     :default (or string "")
     :default-string (or string "")
     :verify-func (if no-check
                    (lambda (string parse-inf)
                      (declare (ignore parse-inf))
                      string)
                    (lambda (string parse-inf)
                      (declare (ignore parse-inf))
                      (and (find string *all-asdf-systems* :test #'string-equal)
                           string)))
     :type :string
     :default-in-prompt nil
     :complete-func 'complete-system)))

(defun prompt-for-asdf-system-with-default (&optional prompt help no-check)
  "Prompts for an ASDF system name and tries to find a default in the
default directory of the current buffer."
  (let* ((directory (editor::buffer-default-directory (editor:current-buffer)))
         (candidate (first (directory (make-pathname :name nil
                                                     :type "asd"
                                                     :defaults directory))))
         (default (and candidate
                       (pathname-name candidate))))
    (prompt-for-asdf-system default prompt help no-check)))

(defun complete-shortcut (string parse-inf)
  "Completion function used by PROMPT-FOR-LISTENER-SHORTCUT."
  (declare (ignore parse-inf))
  (editor::complete-string string (mapcar #'cdr *listener-shortcuts*)
                           :ignore-case t))

(defun find-full-name (abbrev)
  "Given an abbreviation finds the first item in *LISTENER-SHORTCUTS*
that is named by this abbreviation."
  (or (loop for (short . long) in *listener-shortcuts*
            when (string-equal short abbrev)
            do (return long))
      (loop for (nil . long) in *listener-shortcuts*
            when (starts-with-p long abbrev)
            do (return long))))

(defun prompt-for-listener-shortcut ()
  "Prompts for a listener shortcut."
  (let ((input
         (editor::parse-for-something
          :prompt (format nil "Shortcut [~{~A~^,~}] or Command: "
                          (sort (mapcar #'car *listener-shortcuts*) #'string-lessp))
          :must-exist t
          :help (format nil "Type the name or abbreviation of a listener shortcut:~%~%~{~A: ~A~%~}"
                        (loop for (short . long) in *listener-shortcuts*
                              collect short
                              collect long))
          :default ""
          :default-string ""
          :verify-func (lambda (string parse-inf)
                         (declare (ignore parse-inf))
                         (and (find-full-name string)
                              string))
          :type :string
          :default-in-prompt nil
          :complete-func 'complete-shortcut)))
    (find-full-name input)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*handle-warn-on-redefinition* :quiet))
    (defmacro with-input-from-region ((var start end) &body body &environment env)
      "During the evaluation of BODY, VAR is bound to a stream which
returns input from the region denoted by START and END."
      (multiple-value-bind (forms decls)
          (dspec:separate-declarations body env)
        `(let ((,var (pop editor::*free-editor-region-streams*)))
           ,@decls
           (setq ,var 
                 (if ,var
                   (editor::modify-editor-region-stream ,var ,start ,end)
                   (editor::make-editor-region-stream ,start ,end)))
           (unwind-protect
               (progn ,@forms)
             (editor::free-region-stream ,var)))))))

(defmacro with-compilation-environment-at-point ((point &key (compilep nil)
                                                             start-message
                                                             end-message)
                                                        &body body)
  (with-unique-names (buffer)
    `(editor::with-compilation-environment-at-point-fn
      ,point ,start-message ,end-message
      #'(lambda (,buffer)
          (let* ((,(if compilep '*compile-file-pathname* '*load-pathname*)
                  (buffer-pathname ,buffer))
                 (,(if compilep '*compile-file-truename* '*load-truename*)
                  (buffer-pathname ,buffer)) ; buffer-pathname _is_ a truename
                 )
            ,@body)))))

(defun returning-lisp-eval (buffer start end print)
  "Evaluates the region in the buffer BUFFER which is denoted by
START and END and returns the result."
  (with-compilation-environment-at-point (start :start-message "Evaluating..."
                                                :end-message (and (not (editor::windowp print))
                                                                  "Finished evaluating"))
    (with-input-from-region (stream start end)
      (let ((out-stream (if (streamp print)
                          print
                          (editor::buffer-stream buffer)))
            return-value)
        (handler-case
            (progn
              (common-utilities:load-text-stream
               stream
               :exit-load-p t
               :eval-function #'(lambda (form)
                                  (multiple-value-list
                                    (editor::editor-eval buffer form)))
               :print-function #'(lambda (result)
                                   (setq return-value result)
                                   (and print
                                        (if (editor::windowp print)
                                          (process-character
                                           `(message ,editor::*values-format-string* ,result)
                                           print)
                                          (editor::in-output-eval-results out-stream result)))))
              return-value)
          (end-of-file (x)
            (editor::report-region-lisp-eval-error "Incomplete S-expression in region " x)
            (return-from returning-lisp-eval nil))
          (reader-error (x)
            (editor::report-region-lisp-eval-error "Error while reading: ~a " x)
            (return-from returning-lisp-eval nil)))))))

(defmacro with-output-to-help-window ((stream &rest options) &body body)
  "Executes BODY with output that goes to STREAM redirected to an
IDE help window."
  `(editor::with-output-to-help-window-1
       #'(lambda (,stream) ,@body)
     ,@options))

(defun complete-package-name (string parse-inf)
  "Like the function of the same name in the EDITOR package, but
case-insensitive."
  (declare (ignore parse-inf))
  (editor::complete-string
   string
   (sort (loop for pkg in (list-all-packages)
               append (cons (package-name pkg) (package-nicknames pkg)))
         'string<)
   :ignore-case t))


(defun verify-package-func (string parse-inf)
  "Like the function of the same name in the EDITOR package, but
case-insensitive."
  (declare (type editor::parse-inf parse-inf))
  (or (find-package (ignore-errors* (read-from-string (string-upcase string))))
      (if (and (parse-inf-must-exist parse-inf)
	       (not (editor::recursive-parse 'prompt-for-y-or-n
                                             :prompt 
                                             "No such package.  Create it?")))
	  (values nil :no-value)
	(make-package string))))

(defun prompt-for-package* (&key (must-exist  t)
                                 (default *package*)
                                 (prompt  "package: ")
                                 (help  "Type a package name.")
                                 &allow-other-keys)
  "Like EDITOR:PROMPT-FOR-PACKAGE, but case-insensitive."
  (editor::parse-for-something :prompt prompt
                               :must-exist must-exist
                               :help help
                               :default default
                               :verify-func  'verify-package-func
                               :type :keyword
                               :complete-func 'complete-package-name
                               :default default))

(defun clean-namestring (namestring)
  "Replaces characters in NAMESTRING which are illegal for a filename
with underlines.  This function is aimed at Microsoft Windows but
shouldn't do any harm on OS X or Linux."
  (regex-replace-all "[\\\\/?*:<>|\"\\000-\\037]" namestring "_"))

(defun normalize-pathname-for-backup (pathname)
  "Converts the full form of the pathname designator PATHNAME to a
string that is suitable \(modulo illegal characters) as the NAME
component of a filename.  This is a simplified form of what GNU Emacs
does."
  (regex-replace-all "[/\\\\]" 
                     (regex-replace "^([a-zA-Z]):[/\\\\]"
                                    (namestring pathname)
                                    "!drive_\\1!")
                     "!"))

(defun make-backup-filename-using-backup-directory (pathname)
  "Creates and returns a backup pathname for PATHNAME.  Assumes that
*BACKUP-DIRECTORY* denotes a directory.  Note that due to the way the
backup pathname is constructed it is possible that two different files
end up with the same backup filename!"
  (ensure-directories-exist
   (make-pathname :name (clean-namestring
                         (normalize-pathname-for-backup pathname))
                  :type nil
                  :version nil
                  :defaults *backup-directory*)))

(defadvice (editor::make-backup-filename alternative-location :around
                                         :documentation "Circumvents
the original function if the variable *MAKE-BACKUP-FILENAME-FUNCTION*
specifies another function to be used instead.")
    (pathname)
  (cond (*make-backup-filename-function*
         (funcall *make-backup-filename-function* pathname))
        (t (call-next-advice pathname))))
