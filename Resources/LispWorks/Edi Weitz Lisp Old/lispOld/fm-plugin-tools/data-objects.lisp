;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/data-objects.lisp,v 1.14 2010/07/22 09:38:05 edi Exp $

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

(defun fm-data-set-as-text* (self text-data &key source-locale (native-type +k-dttext+))
  "A convenience version of FM-DATA-SET-AS-TEXT."
  (cond (source-locale
         (fm-data-set-as-text self text-data source-locale native-type))
        (t
         (with-locale (locale +k-type-system+)
           (fm-data-set-as-text self text-data locale native-type)))))

(defclass data-object (fm-object)
  ()
  (:documentation "A DATA-OBJECT is a Lisp object which is a
proxy for a FileMaker `Data' object."))

(defmethod fm-delete ((data-object data-object))
  "Deletes the Data \(C++) object which is proxied by
DATA-OBJECT."
  (fm-data-delete (pointer data-object)))

(defun make-data-object ()
  "Creates and returns a DATA-OBJECT."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  (when ptr
                    (ignore-errors
                      (fm-data-delete ptr))))))
      (setq ptr (fm-data-constructor1))
      (make-instance 'data-object :pointer ptr))))

(defmethod as-text-object ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a TEXT-OBJECT."
  (make-instance 'text-object
                 :pointer (fm-data-get-as-text (pointer data-object))
                 :do-not-delete t))

(defmethod (setf as-text-object) ((new-value text-object) (data-object data-object)
                                  &key source-locale (native-type +k-dttext+))
  "Sets the contents of DATA-OBJECT to TEXT-OBJECT."
  (fm-data-set-as-text* (pointer data-object) (pointer new-value)
                        :source-locale source-locale :native-type native-type)
  new-value)

(defmethod as-string ((data-object data-object) &key)
  "Returns the contents of DATA-OBJECT as a Lisp string."
  (as-string (as-text-object data-object)))

(defmethod (setf as-string) ((new-value string) (data-object data-object)
                             &key source-locale (native-type +k-dttext+))
  "Sets the contents of DATA-OBJECT to a TEXT-OBJECT representing the
Lisp string NEW-VALUE."
  (let ((text (make-text-object new-value)))
    (setf (as-text-object data-object
                          :source-locale source-locale
                          :native-type native-type)
          text))
  new-value)

(defmethod as-fix-pt-object ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a FIX-PT-OBJECT."
  (make-instance 'fix-pt-object
                 :pointer (fm-data-get-as-number (pointer data-object))
                 :do-not-delete t))

(defmethod (setf as-fix-pt-object) ((new-value fix-pt-object) (data-object data-object)
                                  &key (native-type +k-dtnumber+))
  "Sets the contents of DATA-OBJECT to FIX-PT-OBJECT."
  (fm-data-set-as-number (pointer data-object) (pointer new-value) native-type)
  new-value)

(defmethod as-integer ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a Lisp integer."
  (as-integer (fm-data-get-as-number (pointer data-object))))

(defmethod (setf as-integer) ((new-value integer) (data-object data-object)
                             &key (native-type +k-dtnumber+))
  "Sets the contents of DATA-OBJECT to a FIX-PT-OBJECT representing the
Lisp integer NEW-VALUE."
  (let ((fix-pt (make-fix-pt-object new-value)))
    (setf (as-fix-pt-object data-object
                           :native-type native-type)
          fix-pt))
  new-value)

(defmethod as-float ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a Lisp float."
  (as-float (fm-data-get-as-number (pointer data-object))))

(defmethod (setf as-float) ((new-value float) (data-object data-object)
                             &key (native-type +k-dtnumber+))
  "Sets the contents of DATA-OBJECT to a FIX-PT-OBJECT representing the
Lisp float NEW-VALUE."
  (let ((fix-pt (make-fix-pt-object new-value)))
    (setf (as-fix-pt-object data-object
                           :native-type native-type)
          fix-pt))
  new-value)

(defmethod as-boolean ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a Lisp boolean."
  (fm-data-get-as-boolean (pointer data-object)))

(defmethod (setf as-boolean) (new-value (data-object data-object)
                             &key (native-type +k-dtnumber+))
  "Sets the contents of DATA-OBJECT to a FIX-PT-OBJECT representing the
\(equivalent of) Lisp generalized boolean NEW-VALUE."
  (setf (as-integer data-object :native-type native-type)
        (if new-value 1 0)))

(defmethod as-date ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a DATE-TIME-OBJECT
representing a date."
  (make-instance 'date-time-object
                 :pointer (fm-data-get-as-date (pointer data-object))
                 :do-not-delete t))

(defmethod (setf as-date) ((new-value date-time-object) (data-object data-object)
                           &key (native-type +k-dtdate+))
  "Sets the contents of DATA-OBJECT to the date represented by
DATE-TIME-OBJECT."
  (fm-data-set-as-date (pointer data-object) (pointer new-value) native-type)
  new-value)

(defmethod as-time ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a DATE-TIME-OBJECT
representing a time of the day."
  (make-instance 'date-time-object
                 :pointer (fm-data-get-as-time (pointer data-object))
                 :do-not-delete t))

(defmethod (setf as-time) ((new-value date-time-object) (data-object data-object)
                           &key (native-type +k-dttime+))
  "Sets the contents of DATA-OBJECT to the time of the day
represented by DATE-TIME-OBJECT."
  (fm-data-set-as-time (pointer data-object) (pointer new-value) native-type)
  new-value)

(defmethod as-timestamp ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a DATE-TIME-OBJECT
representing a timestamp."
  (make-instance 'date-time-object
                 :pointer (fm-data-get-as-time-stamp (pointer data-object))
                 :do-not-delete t))

(defmethod (setf as-timestamp) ((new-value date-time-object) (data-object data-object)
                                &key (native-type +k-dttime-stamp+))
  "Sets the contents of DATA-OBJECT to the timestamp represented
by DATE-TIME-OBJECT."
  (fm-data-set-as-time-stamp (pointer data-object) (pointer new-value) native-type)
  new-value)

(defmethod as-binary-data ((data-object data-object))
  "Returns the contents of DATA-OBJECT as a BINARY-DATA-OBJECT."
  (make-instance 'binary-data-object
                 :pointer (fm-data-get-binary-data (pointer data-object))
                 :do-not-delete t))

(defmethod (setf as-binary-data) ((new-value binary-data-object) (data-object data-object)
                                  &key (force-binary-native-type t))
  "Sets the contents of DATA-OBJECT to BINARY-DATA-OBJECT."
  (fm-data-set-binary-data (pointer data-object) (pointer new-value) force-binary-native-type))

(defmethod get-font-id ((data-object data-object) font-name font-script)
  "Returns the ID of the font identified by \(the Lisp string)
FONT-NAME and the font script FONT-SCRIPT."
  (with-text (text-ptr font-name)
    (fm-data-get-font-id data-object text-ptr font-script (get-environment))))

(defmethod get-font-info ((data-object data-object) font-id)
  "Returns as two values the name and the script of the font with
the ID FONT-ID."
  (with-text (text-ptr "")
    (with-dynamic-foreign-objects ((font-script-ptr (:unsigned :short)))
      (when (fm-data-get-font-info data-object font-id text-ptr font-script-ptr (get-environment))
        (values (fm-text-get-string text-ptr)
                (dereference font-script-ptr))))))
