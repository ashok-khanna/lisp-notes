;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PLUGIN-EXAMPLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/plugin-example/win32.lisp,v 1.6 2010/07/22 09:38:09 edi Exp $

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

;;; The contents of this file are actually not very interesting.  We
;;; do some FLI gymnastics do get at Win32 locale information.  Note
;;; that we don't use `localeconv' from the run-time library like in
;;; the FileMaker example as it seems to return wrong results.

(in-package :plugin-example)

(defun win32-char-type ()
  "Returns the FLI character type to use depending on the Windows
version."
  (if (string= (software-type) "Windows NT")
    :wchar-t :char))

(defun win32-external-format ()
  "Returns the FLI external format to use depending on the Windows
version."
  (if (string= (software-type) "Windows NT")
    :unicode :ascii))

(defconstant +locale-user-default+ #x400
  "Win32 constant for default user locale.")

(defconstant +locale-sdecimal+ #x0e
  "Win32 constant for decimal separator.")

;; wrap the following definitions with EVAL-WHEN, so they're available
;; when the CASE form in SIGNAL-LAST-ERROR is expanded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +error-insufficient-buffer+ 122
    "Win32 identifier for `insufficient buffer' error.")
  (defconstant +error-invalid-flags+ 1004
    "Win32 identifier for `invalid flags' error.")
  (defconstant +error-invalid-parameter+ 87
    "Win32 identifier for `invalid parameter' error."))

(define-foreign-function (%get-locale-info "GetLocaleInfo" :dbcs)
    ((locale (:unsigned :long))
     (lc-type (:unsigned :long))
     (lc-data :pointer)
     (cch-data :int))
  :result-type :int
  :documentation "Win32 function to get locale-specific information.")

(define-foreign-function (%get-last-error "GetLastError")
    ()
  :result-type (:unsigned :long)
  :documentation "Win32 function to get last error.")

(defun signal-last-error ()
  "Gets last error from Windows and signals a corresponding Lisp
error."
  (let* ((error-code (%get-last-error))
         (msg (case error-code
                (#.+error-insufficient-buffer+
                 "Insufficient buffer.")
                (#.+error-invalid-flags+
                 "Invalid flags.")
                (#.+error-invalid-parameter+
                 "Invalid parameter.")
                (otherwise
                 (format nil "Unexpected Win32 error code ~A." error-code)))))
    (error msg)))

(defun get-locale-info (lc-type)
  "Ask Windows for locale-specific information specified by the
integer identifier LC-TYPE and returns the answer as a \(Lisp)
string."
  ;; first call with null pointer and length zero to get length of result
  (let ((length (%get-locale-info +locale-user-default+
                                  lc-type *null-pointer* 0)))
    ;; if length is zero, an error has occured
    (when (zerop length)
      (signal-last-error))
    ;; now allocate foreign string of the right length and call again
    (with-dynamic-foreign-objects ()
      (let ((lc-data (allocate-dynamic-foreign-object :type (win32-char-type)
                                                      :nelems length)))
        (when (zerop (%get-locale-info +locale-user-default+
                                       lc-type
                                       lc-data
                                       length))
          (signal-last-error))
        (convert-from-foreign-string lc-data
                                     :external-format (win32-external-format))))))
