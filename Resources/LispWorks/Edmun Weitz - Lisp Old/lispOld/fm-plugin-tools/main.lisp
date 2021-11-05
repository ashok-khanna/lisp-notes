;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/main.lisp,v 1.27 2010/07/22 09:38:06 edi Exp $

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

(defun fm-expr-env-register-external-function*
       (function-id function-name function-prototype func-ptr
                  &key (min-args 0)
                       (max-args -1)
                       (type-flags #.(logior +k-display-in-all-dialogs+
                                             +k-may-evaluate-on-server+)))
  "This is just a convenience wrapper for FM-EXPR-ENV-REGISTER-EXTERNAL-FUNCTION."
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-register-external-function plugin-id function-id function-name
                                            function-prototype min-args (or max-args -1)
                                            type-flags func-ptr)))

(defun fm-expr-env-un-register-external-function* (function-id)
  "This is just a convenience wrapper for FM-EXPR-ENV-UNREGISTER-EXTERNAL-FUNCTION."
  (with-quadchar (plugin-id *plugin-id*)
    (fm-expr-env-un-register-external-function plugin-id function-id)))

(defun handle-get-string-message (which-string result-size result-ptr)
  "Handles `kFMXT_GetString' messages from FileMaker.
WHICH-STRING is the ID for the information FileMaker wants to
have, RESULT-PTR is where the answer string is supposed to be
stored, and RESULT-SIZE is the maximal size of the result."
  (when-let (string (case which-string
                      (#.+k-fmxt-name-str+ *plugin-name*)
                      (#.+k-fmxt-app-config-str+ *plugin-help-text*)
                      (#.+k-fmxt-options-str+ (create-options-string))))
    #+:win32
    (convert-to-foreign-string string
                               :limit (1- result-size)
                               :external-format :unicode
                               :into (make-pointer :address result-ptr
                                                   :type :wchar-t))
    #-:win32
    (loop with ptr = (make-pointer :address result-ptr
                                   :type :unsigned-short)
          for index from 0 below (1- result-size)
          for char across string
          do (setf (dereference ptr :index index) (char-code char))
          finally (setf (dereference ptr :index index) #\Null))))

(defun register-plugin-functions ()
  "Loops through *PLUGIN-FUNCTIONS* and registers with FileMaker
all functions which were defined with DEFINE-PLUGIN-FUNCTION."
  (let ((prefix (string-append *plugin-id* "_")))
    (dolist (tuple *plugin-functions*)
      (destructuring-bind (function-id prototype c-name min-args max-args flags)
          tuple
        (with-text (name% (string-append prefix (function-name prototype)))
          (with-text (prototype% (string-append prefix (string-trim " " prototype)))
            (let* ((type-flags (or flags
                                   #.(logior +k-display-in-all-dialogs+ +k-may-evaluate-on-server+)))
                   (err-code
                    (fm-expr-env-register-external-function* function-id
                                                             name%
                                                             prototype%
                                                             (make-pointer :symbol-name c-name)
                                                             :min-args min-args
                                                             :max-args max-args
                                                             :type-flags type-flags)))
              (unless (zerop err-code)
                (fm-log "Got error code ~A while registering function ~S.~%"
                        err-code (function-name prototype))))))))))

(defun handle-init-message (version)
  "Handles `kFMXT_Init' messages from FileMaker.  Version is the
database version as sent by FileMaker.  The function is supposed
to return +K-CURRENT-EXTN-VERSION+ if everything is OK."
  (flet ((do-not-enable (condition)
           "Local handler which logs errors during this phase and
refrains from enabling the plug-in."
           (ignore-errors
             (fm-log "Plug-in ~A not enabled: ~A.~%" *plugin-name* condition)
             (when *log-backtraces-p*
               (fm-log "Backtrace:~% ~A~%" (get-backtrace))))
           (return-from handle-init-message +k-do-not-enable+)))
    (handler-bind ((error #'do-not-enable))
      ;; here we connect to the "FMWrapper" shared library (DLL or
      ;; framework) which lives inside the FileMaker folder
      (register-module :fm-wrapper
                       ;; immediate, so we get an error here if
                       ;; something goes wrong
                       :connection-style :immediate
                       :real-name
                       #+:win32 "FMWrapper"
                       ;; relative syntax introduced for dlopen in OS X 10.4
                       #+:macosx "@executable_path/../Frameworks/FMWrapper.framework/Versions/A/FMWrapper")
      ;; check version
      (unless (<= +k70extn-version+ version +k-max-extn-version+)
        (fm-log "Plug-in ~A not enabled because of wrong version ~A.~%"
                *plugin-name* version)
        (return-from handle-init-message +k-bad-extn-version+))
      ;; prepare for Windows registy entries
      (set-product-name)      
      (setf (sys:product-registry-path :fm-plugin-tools)
            (list "Software" *company-name* *product-name*))
      ;; call user-provided init function first if there is one
      (when *init-function*
        (funcall *init-function*))
      ;; register plug-in functions
      (register-plugin-functions)
      +k-current-extn-version+)))

(defun unregister-plugin-functions ()
  "Loops through *PLUGIN-FUNCTIONS* and unregisters with
FileMaker all functions which were defined with
DEFINE-PLUGIN-FUNCTION."
  (dolist (tuple *plugin-functions*)
    (destructuring-bind (function-id prototype &rest rest)
        tuple
      (declare (ignore rest))
      (let ((err-code (fm-expr-env-un-register-external-function* function-id)))
        (unless (zerop err-code)
          (fm-log "Got error code ~A while unregistering function ~S.~%"
                  err-code (function-name prototype)))))))
        
(defun handle-shutdown-message ()
  "Handles `kFMXT_Shutdown' messages from FileMaker."
  ;; unregister plug-in functions
  (unregister-plugin-functions)
  ;; call user-provided shutdown function if there is one
  (when *shutdown-function*
    (funcall *shutdown-function*))
  ;; unregister the :FM-WRAPPER module  
  (disconnect-module :fm-wrapper)
  ;; free global environment if necessary
  (when *global-environment*
    (ignore-errors
      (fm-expr-env-delete *global-environment*)))
  ;; force quit the DLL
  (unless (lw:dll-quit)
    (fm-log "LW:DLL-QUIT didn't succeed.  Diagnostic output follows below.~%")
    (fm-log "Trying \(LW:DLL-QUIT :FORCE T) now.~%")
    (multiple-value-bind (success output)
        (lw:dll-quit :force t :output nil)
      (fm-log "~A" output)
      (fm-log "~&\(LW:DLL-QUIT :FORCE T) ~:[failed~;succeeded~].~%" success))))

(defun handle-app-preferences-message ()
  "Handles `kFMXT_DoAppPreferences' messages from FileMaker."
  ;; we only call the user's function if there is one
  #-:macosx
  (when *preferences-function*
    (funcall *preferences-function*)))

(defun handle-idle-message-internal (idle-level)
  "Handles `kFMXT_Idle' messages from FileMaker.  Calls
HANDLE-IDLE-MESSAGE."
  ;; collect all generations \(but not too often, see *GC-INTERVAL*) if
  ;; the user is idle.
  (when (and (= idle-level +k-fmxt-user-idle+)
             *gc-interval*
             (> (- (get-universal-time) *last-gc*)
                *gc-interval*))
    (gc-generation #+:lispworks-32bit 3
                   #+:lispworks-64bit :blocking-gen-num)
    (setq *last-gc* (get-universal-time)))
  ;; maybe some user code will be called here
  (handle-idle-message idle-level))

(defgeneric handle-idle-message (idle-level)
  (:documentation "Called with corresponding level when FileMaker
is idle.  Can be specialized by plug-in authors."))

(defmethod handle-idle-message (idle-level)
  "The default method which does nothing.")

;; a pointer to the C struct define by PREPARE-FM-PLUGIN-TOOLS
(define-c-typedef fmx-extern-call-ptr
  (:pointer fmx-extern-call-struct))

(define-foreign-callable ("FMExternCallProc" :result-type :void
                                             :calling-convention :cdecl)
    ((parameter-block fmx-extern-call-ptr))
  "The way FileMaker calls into our plug-in.  See FileMaker
documentation for details."
  ;; remember value in global variable, so callbacks have access to it -
  ;; see definition of START-SCRIPT
  (setq *parameter-block* parameter-block)
  ;; dispatch to handlers defined above
  (case (which-call)
    (#.+k-fmxt-get-string+
     (handle-get-string-message (parm1) (parm3) (result)))
    (#.+k-fmxt-idle+
     (handle-idle-message-internal (parm1)))
    (#.+k-fmxt-init+
     (setf (result)
           (handle-init-message (extn-version))))
    (#.+k-fmxt-shutdown+
     (handle-shutdown-message))
    (#.+k-fmxt-do-app-preferences+
     (handle-app-preferences-message))))
