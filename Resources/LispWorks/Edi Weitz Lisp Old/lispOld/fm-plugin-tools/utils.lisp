;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/utils.lisp,v 1.27 2010/07/22 09:38:06 edi Exp $

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :fm-plugin-tools)

(defun check-plugin-id ()
  "Checks whether *PLUGIN-ID* has a valid value.  Should be called in
delivery script."
  (unless (and (stringp *plugin-id*)
               (= 4 (length *plugin-id*))
               (every (lambda (char)
                        (or (char-not-greaterp #\A char #\Z)
                            (char<= #\0 char #\9)))
                      *plugin-id*))
    (error "Plug-in ID must be a string of four characters each of ~
which is a letter or a digit.")))

(defun fm-log (control-string &rest format-args)
  "Utility function which might be useful for debugging a plug-in.
Writes data to the file denoted by *FM-LOGFILE* unless this value
is NIL.  CONTROL-STRING and FORMAT-ARGS are interpreted as by
FORMAT."
  (when *fm-logfile*
    (let ((fm-logfile (if (eq *fm-logfile* t)
                        (merge-pathnames "FM-PlugIn-Tools/fm-plugin-tools.log"
                                         (sys:get-folder-path #-:macosx :local-appdata
                                                              #+:macosx :my-appsupport))
                        *fm-logfile*)))
      (with-open-file (out (ensure-directories-exist fm-logfile)
                           :direction :output
                           :element-type 'lw:simple-char
                           :external-format '(:utf-8 :eol-style :lf)
                           :if-exists :append
                           :if-does-not-exist :create)
        (apply #'format out control-string format-args)
        (finish-output out))))
  (values))

(defun create-options-string ()
  "Creates an option string which can be sent back to FileMaker
on request.  See FileMaker documentation."
  ;; note that character 8 is #\Y although the FileMaker documentation
  ;; says it must always be #\n - go figure...
  (format nil "~A1~:[n~;Y~]nY~:[n~;Y~]nn"
          *plugin-id*
          #+:win32 *preferences-function* #+:macosx nil
          *enable-idle-messages*))

(defun next-function-id ()
  "Returns the next unused function ID, an integer."
  (incf *function-counter*))

(defun next-callable-name ()
  "Returns a symbol which will be used as internal name for a
foreign callable.  Returns a different symbol each time it is
called."
  (gensym "FM-PLUGIN-TOOLS-foreign-callable"))

(defun function-name (prototype)
  "Extracts the function name from a function prototype string
like \"foo ( arg1 ; arg2 )\"."
  (let ((paren-pos (position #\( prototype)))
    (string-trim " " (if paren-pos
                       (subseq prototype 0 paren-pos)
                       prototype))))

(defun plugin-preference (path value-name)
  "Returns preferences \(stored in the Windows registry)
corresponding to PATH and NAME."
  (unless (listp path)
    (setq path (list path)))
  (user-preference path value-name :product :fm-plugin-tools))

(defun (setf plugin-preference) (new-value path value-name)
  "Sets preferences \(stored in the Windows registry)
corresponding to PATH and NAME to the new value NEW-VALUE."
  (unless (listp path)
    (setq path (list path)))
  (setf (user-preference path value-name :product :fm-plugin-tools)
        new-value))

(defmacro remember-interface-geometry (interface-class-name)
  "Convenience macro which sets up a user defined interface class
such that its geometry will automatically be stored in the
Windows registry between different invocations of the plug-in."
  `(progn
     (defmethod capi:top-level-interface-save-geometry-p ((interface ,interface-class-name))
       t)
     (defmethod capi:top-level-interface-geometry-key ((interface ,interface-class-name))
       (values ',interface-class-name :fm-plugin-tools))
     (pushnew ',interface-class-name *symbols-to-keep*)))

(defun get-backtrace ()
  "Returns a full backtrace as a string.  To be used in
handlers."
  (with-output-to-string (out nil :element-type 'lw:simple-char)
    (let ((dbg::*debugger-stack* (dbg::grab-stack nil :how-many most-positive-fixnum))
          (*debug-io* out)
          (dbg:*debug-print-level* nil)
          (dbg:*debug-print-length* nil))
      (dbg:bug-backtrace nil))))

(defun maybe-log-error (cond &optional prototype)
  "Logs the condition COND using FM-LOG if *LOG-ERRORS* is true.
Also logs a backtrace if *LOG-BACKTRACES-P* is true as well."
  (when *log-errors-p*
    (fm-log "Error~:[~*~; in function ~A~]: ~A~%"
            prototype (function-name prototype) cond)
    (when *log-backtraces-p*
      (fm-log "Backtrace:~% ~A~%" (get-backtrace)))))
               
(defun top-level-hook (fn interface)
  "A function which can be used as a top-level hook for
interfaces to make them more robust against unhandled conditions.
See the reference entry for CAPI:INTERFACE."
  (flet ((top-level-error-handler (cond)
           (capi:display-message "~A" cond)
           (maybe-log-error cond)
           (ignore-errors
             (capi:apply-in-pane-process interface
                                         (if (capi:current-dialog-handle)
                                           'capi:abort-dialog
                                           'capi:destroy)
                                         interface))))
    (handler-bind ((error #'top-level-error-handler))
      (funcall fn))))

(defun boolean-value (thing)
  "Returns T if THING is not NIL, NIL otherwise."
  (not (not thing)))

(defun set-product-name ()
  "Sets *PRODUCT-NAME* from *PLUGIN-NAME* if it hasn't been set
explicitly."
  (unless *product-name*
    (setq *product-name* *plugin-name*))
  *product-name*)

(defun version-string ()
  "Returns a string representation of the plug-in version."
  (format nil "~{~A~^.~}" *plugin-version*))

(defun convert-line-endings (string)
  "Converts Mac line endings \(carriage returns, used internally
by FileMaker) to line feeds."
  (with-output-to-string (out nil :element-type 'lw:simple-char)
    (loop for char across string
          do (write-char (case char
                           (#.#\Return #\Linefeed)
                           (otherwise char))
                         out))))

;; this is needed for higher delivery levels
(fli::define-precompiled-foreign-object-accessor-functions
 (((:pointer :void) :no-alloc-p :error :size nil)))

(defmacro with-fmxcpt ((ptr) &body body)
  "Executes BODY with PTR bound to a pointer to a FMXCPT C
struct.  After execution of BODY this C struct's M-CODE slot is
checked for potential error message and a condition is signaled
if appropriate."
  ;; the +K-FOO+ constants will be defined by PREPARE-FM-PLUGIN-TOOLS
  (declare (special +k-no-err+ +k-bad-alloc+ +k-unknown+))
  `(with-dynamic-foreign-objects ((,ptr fmxcpt))
     (setf (foreign-slot-value ,ptr 'm-vers) 1
           (foreign-slot-value ,ptr 'm-code) ,+k-no-err+)
     (prog1
         (progn ,@body)
       (ecase (foreign-slot-value ,ptr 'm-code)
         (,+k-no-err+)
         (,+k-bad-alloc+ (error "FileMaker: Bad allocation."))
         (,+k-unknown+ (error "FileMaker: Exception."))))))

(defmacro define-fmxcpt-function ((lisp-name c-name) arg-list &rest keyword-args)
  "This is basically equivalent to DEFINE-FOREIGN-FUNCTION except
that it implicitly adds a new last argument for a FMXCPT C
struct to the C function definition and wraps the body of the
corresponding Lisp function \(wrapper) with a WITH-FMXCPT for
automatic error handling."
  (with-unique-names (fmxcpt-ptr)
    ;; use a gensym for INTERNAL-NAME that has some resemblance to C-NAME
    (let ((internal-name (gensym c-name))
          (untyped-arg-list (mapcar #'first arg-list)))
      `(progn
         (define-foreign-function (,internal-name ,c-name)
             (,@arg-list
              ;; add pointer to FMXCPT C struct as last argument
              (,fmxcpt-ptr (:pointer fmxcpt)))
           ,@(sys::remove-properties keyword-args '(:lambda-list))
           #+:win32 #+:win32
           :calling-convention :cdecl
           :module :fm-wrapper)
         ;; use lambda list in Lisp function if provided - the
         ;; function LISP-NAME wraps the Lisp function INTERNAL-NAME
         (defun ,lisp-name ,(or (getf keyword-args :lambda-list) untyped-arg-list)
           ;; add error handling
           (with-fmxcpt (,fmxcpt-ptr)
             (,internal-name ,@untyped-arg-list ,fmxcpt-ptr)))))))

;; help the LispWorks IDE to find these definitions
(define-form-parser define-fmxcpt-function (name)
  `(,define-fmxcpt-function ,(first name)))

(define-dspec-alias define-fmxcpt-function (name)
  `(defun ,name))

;; setup correct indentation of DEFINE-FMXCPT-FUNCTION
(editor:setup-indent "define-fmxcpt-function" 2 2 4)

(defun extn-version ()
  "Shortcut to get at EXTN-VERSION slot of current parameter block."
  (foreign-slot-value *parameter-block* 'extn-version))

(defun which-call ()
  "Shortcut to get at WHICH-CALL slot of current parameter block."
  (foreign-slot-value *parameter-block* 'which-call))

(defun parm1 ()
  "Shortcut to get at PARM1 slot of current parameter block."
  (foreign-slot-value *parameter-block* 'parm1))

(defun parm2 ()
  "Shortcut to get at PARM2 slot of current parameter block."
  (foreign-slot-value *parameter-block* 'parm2))

(defun parm3 ()
  "Shortcut to get at PARM3 slot of current parameter block."
  (foreign-slot-value *parameter-block* 'parm3))

(defun result ()
  "Shortcut to get at RESULT slot of current parameter block."
  (foreign-slot-value *parameter-block* 'result))

(defun (setf result) (new-value)
  "Shortcut to change RESULT slot of current parameter block."
  (setf (foreign-slot-value *parameter-block* 'result)
        new-value))

(defun c-start-script ()
  "Shortcut to get at C-START-SCRIPT slot of current parameter block."
  (foreign-slot-value *parameter-block* 'c-start-script))

(defun c-current-env ()
  "Shortcut to get at C-CURRENT-ENV slot of current parameter block."
  (foreign-slot-value *parameter-block* 'c-current-env))

(defun get-environment ()
  "Returns the value of *ENVIRONMENT* or, failing that, the value of
*GLOBAL-ENVIRONMENT*."
  (or *environment*
      ;; use lock so we don't interfere with updates
      (mp:with-lock (*global-environment-lock*)
        *global-environment*)
      (error "No environment available.")))