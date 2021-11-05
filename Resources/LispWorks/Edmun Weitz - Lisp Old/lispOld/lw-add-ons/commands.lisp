;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/commands.lisp,v 1.31 2015/03/06 12:54:25 edi Exp $

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

(defcommand "Insert Space And Show Arglist" (p)
     "Displays arglist of nearest enclosing operator in the echo
area after inserting a space."
     "Displays arglist."
  (self-insert-command p #\Space)
  (show-arglist))

(defcommand "Set Mark And Highlight" (p)
      "Sets the mark and turns on highlighting.  To be used as a
replacement for the normal \"Set Mark\" command if you want
something similar to `transient mark mode.'"
      "Sets the mark and turns on highlighting."
  ;; from Barry Wilkes
  (set-mark-command p)
  (hl-on-command p))

(defcommand "Complete Symbol Without Dialog" (p)
     "Completes the symbol before or around point.  Doesn't pop
up a CAPI dialog window."
     "Completes the symbol before or around point."
  (declare (ignore p))
  (multiple-value-bind (string package)
      (symbol-string-at-point :previous t)
    (multiple-value-bind (completion-set completed-prefix)
        (completions string (package-name package))
      (when (null completion-set)
        (editor-error "No completions for ~S" string))
      (let ((quoted-string (editor::regexp-quote string)))
        (loop until (looking-at quoted-string)
              do (backward-character-command  1)))
      (with-point ((start (current-point))
                   (end (current-point)))
        (increment-point end (length string))
        (let (single-completion-p)
          (recording-for-undo% start end
            (delete-next-character-command (length string))
            (loop for char across completed-prefix
                  do (self-insert-command 1 char))
            (cond ((and (member completed-prefix completion-set :test #'string=)
                        (null (cdr completion-set)))
                   (setq single-completion-p t)
                   (when *insert-right-parenthesis-if-no-args*
                     (maybe-insert-right-parenthesis)))
                  (t
                   (let ((unambiguous-completion-length
                          (loop for c in completion-set
                                minimizing (or (mismatch completed-prefix c)
                                               (length completed-prefix)))))
                     (backward-character-command (- (length completed-prefix)
                                                    unambiguous-completion-length))))))
          ;; this part has to happen without the lock acquired for
          ;; RECORDING-FOR-UNDO% above
          (cond (single-completion-p
                 (editor::clear-echo-area-if-not-current "Sole completion")
                 (sleep .7)
                 (show-arglist))
                (t (show-info
                    (completions-for-echo-area completion-set)))))))))

(defun in-string-p ()
  "Helper function which checks whether we're within a string.  Simply
goes back in steps of one char until it finds a double quote.  Doesn't
check for escaped characters."
  (save-excursion
    (backward-form-command nil)
    (eql #\" (char-before))))

(defcommand "Indent And Complete Symbol" (p)
     "Indents the current line and performs symbol completion.
First indents the line.  If indenting doesn't change the line
point is in, completes the symbol.  If there's no symbol at the
point, shows the arglist for the most recently enclosed macro or
function."
     "Indents the current line and performs symbol completion."
  (let ((line-before (current-line)))
    ;; make sure top-level forms are indented flush left
    (with-point ((line-start (current-point))
                 (line-end (current-point)))
      (line-start line-start)
      (line-end line-end)
      (recording-for-undo% line-start line-end
        (editor::delete-horizontal-space line-start)
        (indent-command p)))
    (when (and (string= line-before (current-line))
               (or (not (string= (editor::buffer-major-mode-name (current-buffer))
                                 "LISP"))
                   (can-move-upwards-p)))
      (let ((char-before (char-before))) 
        (cond ((in-string-p) (expand-file-name-command p))
              ((not (find char-before
                          '(#\( #\) #\Space #\Tab #\Linefeed #\Return #\")))
               (cond #-:editor-does-not-have-abbreviated-complete-symbol
                     (*use-abbreviated-complete-symbol*
                      ;; we need to go to the end of the symbol
                      (let* ((string (symbol-string-at-point :previous t))
                             (quoted-string (editor::regexp-quote string)))
                        (loop until (looking-at quoted-string)
                              do (backward-character-command  1))
                        (increment-point (current-point) (length string)))
                      (editor::abbreviated-complete-symbol-command p))
                     (t (complete-symbol-without-dialog-command p))))
              ((find char-before '(#\Space #\Tab))
               (show-arglist)))))))

(defcommand "Meta Documentation" (p)
     "Finds and displays documentation for the given symbol if it is
supported by Hyperdoc or can be found in one of the online manuals
\(CLHS, LW, MOP).  If point is on a symbol which is known to have
documentation the page is immediately shown.  Otherwise, or with a
prefix argument, the user is queried for the symbol."
     "Shows CLHS/LW/MOP online documentation in browser."
  (let* ((symbol (and (not p)
                      (symbol-at-point :previous t)))
         (string (and symbol
                      (format nil "~:[~;:~]~A"
                              (keywordp symbol)
                              (symbol-name symbol))))
         (uri (and string (doc-entry string))))
    (unless uri
      (let ((*doc-entries* (append (collect-hyperdoc-entries)
                                   *doc-hash-entries*)))
        (setq string (editor::parse-for-something
                      :prompt "Documentation entry for: "
                      :must-exist t
                      :help "Type the symbol you want to see documentation about."
                      :default (or string "")
                      :default-string (or string "")
                      :verify-func (lambda (string parse-inf)
                                     (declare (ignore parse-inf))
                                     (and (doc-entry string)
                                          string))
                      :type :string
                      :default-in-prompt nil
                      :complete-func 'complete-doc-entry)
              uri (doc-entry string))))
    (when (and uri (plusp (length uri)))
      (browse-anchored-uri uri))))
                             
#+:editor-does-not-have-go-back
(defcommand "Pop Definitions Stack" (p)
      "Pops one point from *FIND-DEFINITIONS-STACK* and goes to that
location if the stack wasn't empty.*"
      "Pops one point from definitions stack and goes there."
   (declare (ignore p))
   (let ((point (loop for point = (pop *find-definitions-stack*)
                      while point
                      when (buffer-name (point-buffer point))
                      do (return point))))
     (unless point
       (message "No more point to go.")
       (return-from pop-definitions-stack-command))
     (goto-buffer-point (point-buffer point)
                        point
                        :in-same-window t
                        :warp t)
     (delete-point point)
     (pop-mark-command nil)))

(defcommand "Load ASDF System" (p)
     "Loads an ASDF system \(and compiles it if necessary)."
     "Loads an ASDF system."
  (declare (ignore p))
  (when-let (name (prompt-for-asdf-system-with-default "Load ASDF System: "))
    (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'asdf:oos 'asdf:load-op name)))

(defcommand "Compile ASDF System" (p)
     "Compiles an ASDF system \(and compiles it if necessary)."
     "Compiles an ASDF system."
  (declare (ignore p))
  (when-let (name (prompt-for-asdf-system-with-default "Compile ASDF System: "))
    (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'asdf:oos 'asdf:compile-op name)))

(defcommand "Test ASDF System" (p)
  "Tests an ASDF system \(and compiles it if necessary)."
  "Tests an ASDF system."
  (declare (ignore p))
  (when-let (name (prompt-for-asdf-system-with-default "Test ASDF System: "))
    (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'asdf:operate 'asdf:test-op name :force t)))

(defcommand "Invoke Listener Shortcut" (p)
     "Prompts for a listener shortcut and invokes the corresponding command."
     "Prompts for a listener shortcut and invokes it."
  (let* ((command-name (prompt-for-listener-shortcut))
         (command (and command-name
                       (editor::getstring command-name editor::*command-names*))))
    (when command
      (editor::funcall-command command p))))

(defcommand "Maybe Invoke Listener Shortcut" (p)
     "Like \"Invoke Listener Shortcut\" but works only if point is in
a listener window immediately after the last prompt with no input
after it.  Otherwise insert a comma."
     "Restricted version of \"Invoke Listener Shortcut\"."
  (cond ((eq (buffer-flag (current-buffer)) :listener)
         (let* ((stream (editor:variable-value 'editor::rubber-stream))
                (start (editor::editor-region-stream-start stream))
                (end (editor::editor-region-stream-end stream)))
           (cond ((and (point= start (current-point))
                       (point= start end))
                  (invoke-listener-shortcut-command p))
                 (t (self-insert-command p #\,)))))
        (t (self-insert-command p #\,))))

(defcommand "Change Package" (p)
     "Prompts for a package and invokes IN-PACKAGE in listener.  Works
only if in listener."
     "Prompts for a package and invokes IN-PACKAGE in listener."
  (declare (ignore p))
  (when (eq (buffer-flag (current-buffer)) :listener)
    (let ((package (prompt-for-package* :prompt "Package: "
                                        :must-exist t)))
      (when package
        (editor::execute-listener-command 'in-package (package-name package))))))

(defcommand "Change Directory" (p)
     "Changes the default directory and *DEFAULT-PATHNAME-DEFAULTS*."
     "Changes default directory."
  (declare (ignore p))
  (let ((directory (prompt-for-directory)))
    (when directory
      (setq *default-pathname-defaults*
            (change-directory directory)))))

(defcommand "Show Directory" (p)
     "Shows the default directory in the echo area."
     "Shows default directory."
  (declare (ignore p))
  (show-info (namestring (get-working-directory))))

(defcommand "Quit" (p)
     "Quits image without asking for confirmation."
     "Quits image immediately."
  (declare (ignore p))
  (quit))

(defcommand "Tools Listener" (p)
     "Like menu Works > Tools > Listener."
     "Like menu Works > Tools > Listener."
  ;; from Dmitri Ivanov
  (declare (ignore p))
  (capi:find-interface 'lw-tools:listener))

(defcommand "Tools Editor" (p)
     "Like menu Works > Tools > Editor."
     "Like menu Works > Tools > Editor."
  ;; from Dmitri Ivanov
  (declare (ignore p))
  (capi:find-interface 'lw-tools:editor))

(defcommand "Tools Apropos" (p)
     "Shows Apropos Dialog."
     "Shows Apropos Dialog."
  (declare (ignore p))
  (capi:find-interface 'apropos-dialog))

(defcommand "Untrace All" (p)
     "Untraces all traced definitions."
     "Untraces all traced definitions."
  (declare (ignore p))
  (untrace))

(defcommand "Toggle Trace" (p &optional name)
     "Toggles Trace."
     "Traces or Untraces the given function."
  (let ((name (or name
                  (and (not p)
                       (symbol-at-point :previous t))
                  (prompt-for-symbol p :prompt "Symbol to Trace: "))))
    (flet ((traced ()
             (member name (eval '(trace)))))
      (cond ((traced)
             (eval `(untrace ,name)))
            (t
             (eval `(trace ,name))))
      (show-info (format nil "~A is now ~@[un~]traced."
                         name (not (traced)))))))

(defcommand "Evaluate Last Form And Inspect" (p &optional (point (current-point)))
     "Evaluates Lisp form before the current point.  The result
is inspected in an IDE Inspector."
     "Evaluates Lisp form before point and inspects result."
  (declare (ignore p))
  (with-point ((start point)
               (end point))
    (unless (editor:form-offset start -1 t 0)
      (editor-error "Cannot find start of the form to evaluate"))
    (let ((buffer (editor:point-buffer start)))
      (let ((value (returning-lisp-eval buffer start end
                                        (editor::current-echo-area-window))))
        (gui-inspect (case (length value)
                       (1 (car value))
                       (t value)))))))

(defcommand "Evaluate Last Form And Describe" (p &optional (point (current-point)))
     "Evaluates Lisp form before the current point.  The result is
described in a help window."
     "Evaluates Lisp form before point and describes result."
  (declare (ignore p))
  (with-point ((start point)
               (end point))
    (unless (editor:form-offset start -1 t 0)
      (editor-error "Cannot find start of the form to evaluate"))
    (let ((buffer (editor:point-buffer start)))
      (let ((values (returning-lisp-eval buffer start end
                                         (editor::current-echo-area-window))))
        (with-compilation-environment-at-point ((current-point))
          (with-output-to-help-window (*standard-output*)
            (dolist (val values)
              (describe val)
              (terpri))))))))

#+:quicklisp
(defcommand "Quickload Library" (p)
     "Load a library with Quicklisp (see http://www.quicklisp.org)."
     "Load Library with Quicklisp."
  (declare (ignore p))
  (when-let (name (prompt-for-asdf-system-with-default
                   "Library to open with Quicklisp: "
                   "Type a name of an ASDF system or a Quicklisp-loadable library."
                   t))
    (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'ql:quickload name)))

#+:quicklisp
(defcommand "Quicklisp Update Client" (p)
     "Update Quicklisp Client"
     "Update Quicklisp Client"
  (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'ql:update-client))

#+:quicklisp
(defcommand "Quicklisp Update All Dists" (p)
      "Update all Quicklisp dists"
      "Update all Quicklisp dists"
  (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'ql:update-all-dists))
