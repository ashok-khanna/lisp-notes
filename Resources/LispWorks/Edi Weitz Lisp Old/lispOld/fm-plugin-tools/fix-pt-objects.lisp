;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/fix-pt-objects.lisp,v 1.7 2010/07/22 09:38:05 edi Exp $

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

(defun fm-fix-pt-constructor1* (&key (val 0) (precision +k-deflt-fixed-precision+))
  "A convenience version of FM-FIX-PT-CONSTRUCTOR1."
  (fm-fix-pt-constructor1 val precision))

(defmacro with-fix-pt ((ptr number &key (precision '+k-deflt-fixed-precision+)) &body body)
  "Executes BODY with PTR bound to a fresh FixPt object
intialized with NUMBER and using the precision PRECISION.  The
object is guaranteed to be deleted after the execution of BODY."
  (rebinding (number)
    `(let (,ptr)
       (unwind-protect
           (progn
             (setq ,ptr (fm-fix-pt-constructor1* :precision ,precision))
             (cond ((floatp ,number)
                    (fm-fix-pt-assign-double ,ptr ,number))
                   ((integerp ,number)
                    (fm-fix-pt-assign-int ,ptr ,number)))
             ,@body)
         (when ,ptr
           (ignore-errors
             (fm-fix-pt-delete ,ptr)))))))

(defclass fix-pt-object (fm-object)
  ()
  (:documentation "A FIX-PT-OBJECT is a Lisp object which is a
proxy for a FileMaker `FixPt' object."))

(defmethod fm-delete ((fix-pt-object fix-pt-object))
  "Deletes the FixPt \(C++) object which is proxied by
TEXT-OBJECT."
  (fm-fix-pt-delete (pointer fix-pt-object)))

(defun make-fix-pt-object (&key (val 0) (precision +k-deflt-fixed-precision+))
  "Creates and returns a FIX-PT-OBJECT representing the number
VAL with precision PRECISION."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  (when ptr
                    (ignore-errors
                      (fm-fix-pt-delete ptr))))))
      (setq ptr (fm-fix-pt-constructor1* val precision))
      (make-instance 'fix-pt-object :pointer ptr))))

(defmethod as-integer ((fix-pt-object fix-pt-object))
  "Returns the number represented by FIX-PT-OBJECT as an integer."
  (fm-fix-pt-as-long (pointer fix-pt-object)))

(defmethod (setf as-integer) ((new-value integer) (fix-pt-object fix-pt-object) &key)
  "Sets the number represented by FIX-PT-OBJECT to NEW-VALUE, a
Lisp integer."
  (fm-fix-pt-assign-int (pointer fix-pt-object) new-value)
  new-value)

(defmethod as-float ((fix-pt-object fix-pt-object))
  "Returns the number represented by FIX-PT-OBJECT as a float."
  (fm-fix-pt-as-float (pointer fix-pt-object)))

(defmethod (setf as-float) ((new-value float) (fix-pt-object fix-pt-object) &key)
  "Sets the number represented by FIX-PT-OBJECT to NEW-VALUE, a
Lisp float."
  (fm-fix-pt-assign-double (pointer fix-pt-object) new-value)
  new-value)

(defmethod as-boolean ((fix-pt-object fix-pt-object))
  "Returns the number represented by FIX-PT-OBJECT as a boolean."
  (fm-fix-pt-as-bool (pointer fix-pt-object)))

(defmethod (setf as-boolean) (new-value (fix-pt-object fix-pt-object) &key)
  "Sets the number represented by FIX-PT-OBJECT to \(the
FileMaker equivalent of) NEW-VALUE, a generalized Lisp boolean."
  (setf (as-integer fix-pt-object) (if new-value 1 0)))

(defmethod precision ((fix-pt-object fix-pt-object))
  "Returns the precision of FIX-PT-OBJECT."
  (fm-fix-pt-get-precision (pointer fix-pt-object)))

(defmethod (setf precision) ((new-value integer) (fix-pt-object fix-pt-object))
  "Sets the precision of FIX-PT-OBJECT."
  (fm-fix-pt-set-precision (pointer fix-pt-object) new-value)
  new-value)