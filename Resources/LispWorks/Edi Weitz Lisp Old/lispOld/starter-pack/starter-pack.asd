;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/starter-pack.asd,v 1.58 2011/02/11 19:56:05 edi Exp $

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

(in-package :cl-user)

(defvar *starter-pack-version* '(0 6 2)
  "Version number.  Should be a list of three non-negative integers,
e.g. (4 2 1) would correspond to version \"4.2.1\".")

(defun starter-pack-version-string ()
  "Returns a string representation of the plug-in version."
  (format nil "~{~A~^.~}" *starter-pack-version*))

;; the function must be compiled for the delivered executable
(compile 'starter-pack-version-string)
;; we export its name so we can import it later
(export 'starter-pack-version-string)

(defpackage :starter-pack-asd
  (:use :cl :asdf))

(in-package :starter-pack-asd)

(defsystem :starter-pack
  :serial t
  :version #.(cl-user:starter-pack-version-string)
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "libs")
               (:file "archive")
               (:file "http")
               (:file "interface")
               (:file "main"))
  :depends-on (:archive :zip :gzip-stream :cl-fad :drakma :cl-ppcre))
