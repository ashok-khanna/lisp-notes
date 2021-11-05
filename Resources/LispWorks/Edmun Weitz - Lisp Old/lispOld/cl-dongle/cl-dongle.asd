;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/cl-dongle.asd,v 1.17 2008/05/01 14:47:40 edi Exp $

;;; Copyright (c) 2008, Dr. Edmund Weitz.  All rights reserved.

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

(defpackage :cl-dongle-asd
  (:use :cl :asdf))

(in-package :cl-dongle-asd)

(defvar *cl-dongle-version-string* "0.1.2")

(asdf:defsystem :cl-dongle
  :serial t
  :version #.*cl-dongle-version-string*
  :depends-on (:lw-win)
  :components ((:file "packages")
               (:file "specials")
               (:file "conditions")
               (:file "util")
               (:file "fli")
               (:file "serialize")
               (:file "api")))

(asdf:defsystem :cl-dongle-test
  :serial t
  :version #.*cl-dongle-version-string*
  :depends-on (:cl-dongle)
  :components ((:module test
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "admin")
                             (:file "crypt")
                             (:file "store")
                             (:file "sign")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-dongle))))
  (operate 'load-op 'cl-dongle-test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :cl-dongle-test))))


