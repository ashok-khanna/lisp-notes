;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/locale-objects.lisp,v 1.7 2010/07/22 09:38:06 edi Exp $

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

(defmacro with-locale ((ptr input-type) &body body)
  "Executes BODY with PTR bound to a fresh Locale object with
input type INPUT-TYPE.  The object is guaranteed to be deleted
after the execution of BODY."
  `(let (,ptr)
     (unwind-protect
         (progn
           (setq ,ptr (fm-locale-constructor1 ,input-type))
           ,@body)
       (when ,ptr
         (ignore-errors
           (fm-locale-delete ,ptr))))))

(defclass locale-object (fm-object)
  ()
  (:documentation "A LOCALE-OBJECT is a Lisp object which is a
proxy for a FileMaker `Locale' object."))

(defmethod fm-delete ((locale-object locale-object))
  "Deletes the Locale \(C++) object which is proxied by
LOCALE-OBJECT."
  (fm-locale-delete (pointer locale-object)))

(defun make-locale-object (&optional (input-type +k-type-system+))
  "Creates and returns a LOCALE-OBJECT with the input type
INPUT-TYPE."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  ;; delete object if something bad happens during
                  ;; creation
                  (when ptr
                    (ignore-errors
                      (fm-locale-delete ptr))))))
      (setq ptr (fm-locale-constructor1 input-type))
      (make-instance 'locale-object :pointer ptr))))