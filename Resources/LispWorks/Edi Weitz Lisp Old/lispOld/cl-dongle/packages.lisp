;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/packages.lisp,v 1.13 2008/04/27 20:11:53 edi Exp $

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

(defpackage :cl-dongle
  (:use :cl :fli :sys :dspec)
  (:add-use-defaults t)
  (:export :*demo-authentication-code*
           :*product-id*
           :assert-dongle
           :authenticate
           :authentication-failed
           :authentication-required
           :counter-value
           :dongle-decrypt
           :dongle-decrypt
           :dongle-decrypt-to-lisp-object
           :dongle-decrypt-to-string
           :dongle-encrypt
           :dongle-encrypt
           :dongle-encrypt-lisp-object
           :dongle-encrypt-string
           :dongle-error
           :dongle-hardware-version
           :dongle-interface
           :dongle-memory-size
           :dongle-not-found
           :dongle-number-of-counters
           :dongle-number-of-keys
           :dongle-present-p
           :dongle-serial-number
           :dongle-software-version
           :dongle-type
           :function-not-supported
           :lpt-open-error
           :lpt-port-busy
           :make-static-u32-array
           :no-lpt-port-found
           :parameter-invalid
           :product-id
           :random-key
           :retrieve-data
           :retrieve-lisp-object
           :retrieve-string
           :sign
           :sign-lisp-object
           :signature-invalid
           :store-data
           :store-lisp-object
           :store-string
           :tea-decrypt
           :tea-decrypt-to-lisp-object
           :tea-decrypt-to-string
           :tea-encrypt
           :tea-encrypt-lisp-object
           :tea-encrypt-string
           :unknown-return-value
           :usb-port-busy
           :write-key))
