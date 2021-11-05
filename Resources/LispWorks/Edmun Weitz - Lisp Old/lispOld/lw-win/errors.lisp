;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-WIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-win/errors.lisp,v 1.12 2010/08/10 17:31:01 edi Exp $

;;; Copyright (c) 2008-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :lw-win)

(define-foreign-function (%get-last-error "GetLastError")
    ()
  :result-type :unsigned-long
  :documentation "Win32 function to retrieve the calling thread's last
error code.")

(define-condition win32-condition (condition)
  ((code :initarg :code
         :reader error-code
         :documentation "The original error code \(an integer) of the
Win32 API.")
   (message :initarg :message
            :reader error-message
            :documentation "The corresponding error message - see
MESSAGE-FOR-ERROR-CODE below."))
  (:report (lambda (condition stream)
             (write-string (error-message condition) stream)))
  (:documentation "Conditions of this type are signalled when errors
resulting from calls into the Win32 API are caught.  The conditions
contain the original numerical error code as well as a human-readable
string explaining the type of the error."))

(define-condition win32-error (win32-condition error)
  ()
  (:documentation "A subclass of WIN32-CONDITION for errors."))

(define-condition win32-warning (win32-condition warning)
  ()
  (:documentation "A subclass of WIN32-CONDITION for warnings."))

(define-foreign-function (%format-message "FormatMessage" :dbcs)
    ((flags dword)
     (source :pointer)
     (message-id dword)
     (language-id dword)
     (buffer (:reference-return :pointer))
     (size dword)
     (arguments :pointer))
  :lambda-list (message-id &key
                           (flags (logior +format-message-allocate-buffer+
                                          +format-message-ignore-inserts+
                                          +format-message-from-system+))
                           (source *null-pointer*)
                           (language-id 0)
                           (size 0)
                           (arguments *null-pointer*)
                           &aux buffer)
  :result-type dword)

(define-foreign-function (%local-free "LocalFree")
    ((mem hlocal))
  :result-type hlocal)

(defun message-for-error-code (error-code)
  "Returns a Lisp string containing an error message corresponding to
the Win32 error code ERROR-CODE."
  (let (length message-ptr)
    (unwind-protect
        (progn
          ;; the results of this function depend, amongst other
          ;; things, on the default values for the optional arguments
          ;; of %FORMAT-MESSAGE, so please see there and MSDN
          ;; documentation
          (multiple-value-setq (length message-ptr) (%format-message error-code))
          (when (zerop length)
            (let ((last-error-code (%get-last-error)))
              (error 'win32-error
                     :code last-error-code
                     :message (format nil "Got error ~A while trying to retrieve message for error code ~A."
                                      last-error-code error-code))))
          ;; remove potential line breaks from end
          (string-right-trim
           '(#\Newline)
           (convert-from-foreign-string message-ptr :external-format (win32-external-format))))
      ;; cleanup part of UNWIND-PROTECT form - free memory used for
      ;; the message
      (when message-ptr
        (unless (null-pointer-p (%local-free message-ptr))
          (let ((last-error-code (%get-last-error)))
            (error 'win32-error
                   :code last-error-code
                   :message (format nil "Got error ~A while trying to free local memory of ~S."
                                    last-error-code message-ptr))))))))                    

(defun signal-last-error (&key error-code warnp format)
  "Retrieves the last error code from Windows \(or uses ERROR-CODE if
supplied) and signals a corresponding Lisp error of type WIN-32-ERROR
\(or a warning of type WIN-32-WARNING if WARNP is true).

Uses FORMAT \(if supplied) to format the error message."
  (let* ((code (or error-code (%get-last-error)))
         (message (message-for-error-code code)))    
    (when format
      (setq message (format nil format message)))
    (cond (warnp (warn 'win32-warning
                       :code code
                       :message message))
          (t (error 'win32-error
                    :code code
                    :message message)))))

(defmacro with-error-check (() &body body)
  "Executes BODY and returns the result of the last form of body, but
first checks this result.  If it is the number zero \(0), a null
pointer, or NIL, SIGNAL-LAST-ERROR is called."
  (with-unique-names (result)
    `(let ((,result (progn ,@body)))
       (when (or (and (pointerp ,result)
                      (null-pointer-p ,result))
                 (and (numberp ,result)
                      (zerop ,result))
                 (null ,result))
         (signal-last-error))
       ,result)))