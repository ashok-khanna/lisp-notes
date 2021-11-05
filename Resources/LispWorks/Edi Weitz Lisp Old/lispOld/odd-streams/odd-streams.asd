;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/odd-streams.asd,v 1.8 2013/01/11 17:14:20 edi Exp $

;;; Copyright (c) 2007-2013, Dr. Edmund Weitz.  All rights reserved.

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

(defpackage :odd-streams-system
  (:use :asdf :cl))

(in-package :odd-streams-system)

(defsystem :odd-streams
  :version "0.1.2"
  :serial t
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "conditions")
               (:file "stream")
               (:file "buffer")
               (:file "output")
               (:file "input"))
  :depends-on (:trivial-gray-streams))

(defsystem :odd-streams-test
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "util")
                                     (:file "tests"))))
  :depends-on (:odd-streams))

(defmethod perform ((o test-op) (c (eql (find-system 'odd-streams))))
  (operate 'load-op 'odd-streams-test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :odd-streams-test))))
