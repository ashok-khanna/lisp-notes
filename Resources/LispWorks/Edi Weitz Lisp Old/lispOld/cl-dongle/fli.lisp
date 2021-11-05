;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/fli.lisp,v 1.17 2008/05/01 14:47:41 edi Exp $

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

(in-package :cl-dongle)

(register-module :sglw32 :real-name "SglW32")

(defmacro define-sg-api-function ((lisp-name c-name) arg-list &rest other-args)
  "Utility macro which defines a Lisp foreign function for the C
function C-NAME and wraps it with a Lisp function LISP-NAME which
checks the primary return value, signals an error if necessary and
otherwise returns the other return values.

ARG-LIST and OTHER-ARGS are as in DEFINE-FOREIGN-FUNCTION, but only
the subset used below is catered for."
  (let* ((return-value (gensym "RETURN-VALUE"))
         ;; use a gensym for INTERNAL-NAME that has some resemblance
         ;; to C-NAME
         (internal-name (gensym c-name))
         ;; all variables which are declared to be of some
         ;; :REFERENCE-RETURN foreign type
         (reference-returns (loop for (var type) in arg-list
                                  when (and (eq var :ignore)
                                            (listp type)
                                            (eq (first type) :reference-return))
                                  collect (gensym)))
         (untyped-arg-list (remove :ignore (remove :constant (mapcar 'first arg-list))))
         (lisp-lambda-list (or (getf other-args :lambda-list)
                               untyped-arg-list))
         (lisp-arguments (loop for parameter in lisp-lambda-list
                               unless (eq parameter '&optional)
                               when (listp parameter)
                               collect (first parameter)
                               else
                               collect parameter)))
    `(progn
       (define-foreign-function (,internal-name ,c-name)
           ,arg-list
         :result-type :unsigned-long
         :module :sglw32
         ,@other-args)
       (defun ,lisp-name ,lisp-lambda-list
         ,@(when-let (documentation (getf other-args :documentation))
             (list documentation))
         (multiple-value-bind (,return-value ,@reference-returns)
             (,internal-name ,@lisp-arguments)
           (check-return-value ,return-value)
           (values ,@reference-returns))))))
           
;; help the LispWorks IDE to find these definitions
(define-form-parser define-sg-api-function (name)
  `(,define-sg-api-function ,(first name)))

(define-dspec-alias define-sg-api-function (name)
  `(defun ,name))

;; setup correct indentation of DEFINE-SG-API-FUNCTION
(editor:setup-indent "define-sg-api-function" 2 2 4)

(define-sg-api-function (assert-dongle "SglSearchLock")
    ((product-id :unsigned-long))
  :lambda-list (&optional (product-id *product-id*))
  :documentation "Checks if the dongle with the product ID PRODUCT-ID
is present.  Signals a DONGLE-NOT-FOUND error if it is not, returns no
value if successful.

See also DONGLE-PRESENT-P.")

(define-sg-api-function (dongle-serial-number "SglReadSerialNumber")
    ((product-id :unsigned-long)
     ;; serial number
     (:ignore (:reference-return :unsigned-long)))
  :lambda-list (&optional (product-id *product-id*))
  :documentation "Returns the serial number for the dongle with the
product ID PRODUCT-ID.")

(define-sg-api-function (product-id "SglReadProductId")
    ;; product-id
    ((:ignore (:reference-return :unsigned-long)))
  :documentation "Returns the \(first) dongle's product ID.")

;; functions without docstrings are exactly equivalent to their C
;; counterparts
(define-sg-api-function (sgl-write-product-id "SglWriteProductId")
    ((old-product-id :unsigned-long)
     (new-product-id :unsigned-long))
  :lambda-list (new-product-id &optional (old-product-id *product-id*)))

;; this is used for the return value of SglReadConfig - see SglWin32.h
;; for details
(define-c-struct sgl-config
  (type :unsigned-long)
  (interface :unsigned-long)
  (software-version :unsigned-long)
  (hardware-version :unsigned-long)
  (serial-number :unsigned-long)
  (memory-size :unsigned-long)
  (number-of-counters :unsigned-long)
  (number-of-keys :unsigned-long))

(define-sg-api-function (sgl-read-config "SglReadConfig")
    ((product-id :unsigned-long)
     (:constant +sgl-read-config-lock-info+ :unsigned-long)
     (data (:pointer sgl-config)))
  :lambda-list (data &optional (product-id *product-id*)))

(defun read-config (product-id slot-name)
  "Wrapper for SGL-READ-CONFIG which temporarily sets up a SGL-CONFIG
C struct and returns the value corresponding to the slot named
SLOT-NAME."
  (with-dynamic-foreign-objects ((config sgl-config))
    (sgl-read-config config product-id)
    (foreign-slot-value config slot-name)))

(define-sg-api-function (sgl-read-data "SglReadData")
    ((product-id :unsigned-long)
     (address :unsigned-long)
     (count :unsigned-long)
     (data (:pointer :unsigned-long)))
  :lambda-list (address count data &optional (product-id *product-id*)))

(define-sg-api-function (sgl-write-data "SglWriteData")
    ((product-id :unsigned-long)
     (address :unsigned-long)
     (count :unsigned-long)
     (data (:pointer :unsigned-long)))
  :lambda-list (address count data &optional (product-id *product-id*)))

(define-sg-api-function (sgl-read-counter "SglReadCounter")
    ((product-id :unsigned-long)
     (cnt-num :unsigned-long)
     ;; data
     (:ignore (:reference-return :unsigned-long)))
  :lambda-list (cnt-num &optional (product-id *product-id*)))

(define-sg-api-function (sgl-write-counter "SglWriteCounter")
    ((product-id :unsigned-long)
     (cnt-num :unsigned-long)
     (data :unsigned-long))
  :lambda-list (cnt-num data &optional (product-id *product-id*)))

(define-sg-api-function (sgl-crypt-lock "SglCryptLock")
    ((product-id :unsigned-long)
     (key-num :unsigned-long)
     (crypt-mode :unsigned-long)
     (block-cnt :unsigned-long)
     (data (:pointer :unsigned-long)))
  :lambda-list (key-num crypt-mode block-cnt data &optional (product-id *product-id*)))

(define-sg-api-function (sgl-write-key "SglWriteKey")
    ((product-id :unsigned-long)
     (keynum :unsigned-long)
     (key (:pointer :unsigned-long)))
  :lambda-list (keynum key &optional (product-id *product-id*)))

(define-sg-api-function (sgl-authent-a "SglAuthentA")
    ((authent-code (:pointer :unsigned-long))
     (app-rand-num (:pointer :unsigned-long))
     (lib-rand-num (:pointer :unsigned-long))))

(define-sg-api-function (sgl-authent-b "SglAuthentB")
    ((lib-rand-num (:pointer :unsigned-long))))

