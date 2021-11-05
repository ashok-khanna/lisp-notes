;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/http.lisp,v 1.20 2011/02/11 19:56:05 edi Exp $

;;; Copyright (c) 2006-2011, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :starter-pack)

(defun get-file-from-url (url)
  "Retrieves the file from URL and copies it to a temporary file
the pathname of which is returned."
  (let (status-code stream)
    (handler-case 
        (unwind-protect
            (progn
              (multiple-value-setq (stream status-code)
                  (drakma:http-request url
                                       :user-agent (user-agent-string)
                                       :protocol :http/1.0
                                       :proxy (get-proxy url)
                                       :want-stream t))
              (unless (= status-code 200)
                (error "Response code was not 200."))
              (setf (flexi-stream-element-type stream) 'octet)
              (with-open-temp-file (out (suffix url))
                (fad:copy-stream stream out nil)))
          ;; make sure the stream is always closed
          (when stream
            (ignore-errors (close stream))))
      (error (cond)
        (error "An error occurred while trying to get ~S:~%~A."
               url cond)))))
            
(defun get-asdf ()
  "Retrieves the `asdf.lisp' file and copies it to the library
directory."
  (let ((target (merge-pathnames "asdf.lisp" (lib-dir))))
    (fad:copy-file (get-file-from-url *asdf-url*)
                   (ensure-directories-exist target))))

(defun get-sqlite ()
  "Retrieves the SQLite3 DLL and copies it to the library
directory \(after unpacking the enclosing ZIP archive)."
  (extract-archive (get-file-from-url *sqlite-url*))
  (ignore-errors
    ;; remove unneeded .DEF file
    (delete-file (merge-pathnames "sqlite3.def" (lib-dir)))))

(defun get-start-file ()
  "Retrieves the `start.lisp' file and copies it to the library
directory."
  (let ((target (merge-pathnames "start.lisp" (lib-dir))))
    (fad:copy-file (get-file-from-url *start-file-url*)
                   (ensure-directories-exist target))))

(defun get-lib (lib &optional status-function)
  "Retrieves the library denoted by LIB from the Internet,
unpacks the archive, and copies the contents to the library
directory.  If STATUS-FUNCTION is provided, it is used to
generate messages about what currently happens."
  (when status-function
    (funcall status-function "Downloading ~A." (name lib)))
  (let ((archive (get-file-from-url (url lib))))
    (when status-function
      (funcall status-function "Installing ~A." (name lib)))
    (extract-archive archive)))

(defun get-libs (&optional status-function)
  "Uses GET-LIB to retrieve all libraries in *LIBS* in turn."
  (dolist (lib *libs*)
    (when (or (checked lib) (required lib))
      (get-lib lib status-function))))