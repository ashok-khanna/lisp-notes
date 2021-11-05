;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/binary-data-objects.lisp,v 1.13 2010/07/22 09:38:05 edi Exp $

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

(defun quadchar-to-string (quadchar-ptr)
  "Returns the four character string which is represented by the
QuadChar object pointed to by QUADCHAR-PTR."
  (coerce (loop for i below 4
                collect (fm-quad-char-operator-ar quadchar-ptr i))
          'string))

(defmacro with-quadchar ((ptr &optional string) &body body)
  "Executes BODY with PTR pointing to a new QuadChar object which is
guaranteed to be deleted after the execution of BODY.  If STRING is
not NIL it is supposed to be a four-character string, and the
QuadChar object will represent this string."
  (rebinding (string)
    `(let (,ptr)
       (unwind-protect
           (progn
             (setq ,ptr (if ,string
                          (fm-quad-char-constructor2 (char ,string 0)
                                                     (char ,string 1)
                                                     (char ,string 2)
                                                     (char ,string 3))
                          (fm-quad-char-constructor1)))
             ,@body)
         (when ,ptr
           (ignore-errors
             (fm-quad-char-delete ,ptr)))))))

(defun fm-binary-data-get-data* (self index offset amount buffer)
  "Like FM-BINARY-DATA-GET-DATA, but with error checking."
  (let ((err-code (fm-binary-data-get-data self index offset amount buffer)))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_BinaryData_GetData."
             err-code))))

(defun fm-binary-data-add* (self data-type amount buffer)
  "Like FM-BINARY-DATA-ADD, but with error checking."
  (let ((err-code (fm-binary-data-add self data-type amount buffer)))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_BinaryData_Add."
             err-code))))

(defun fm-binary-data-get-fnam-data* (self file-path-list)
  "Like FM-BINARY-DATA-GET-FNAMDATA, but with error checking."
  (let ((err-code (fm-binary-data-get-fnamdata self file-path-list)))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_BinaryData_GetFNAMData."
             err-code))))

(defun fm-binary-data-add-fnam-data* (self file-path-list)
  "Like FM-BINARY-DATA-ADD-FNAMDATA, but with error checking."
  (let ((err-code (fm-binary-data-add-fnamdata self file-path-list)))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_BinaryData_AddFNAMData."
             err-code))))

(defun fm-binary-data-get-size-data* (self)
  "Wraps FM-BINARY-DATA-GET-SIZEDATA to make it more Lisp-like.
Returns the result as two values and checks for errors."
  (multiple-value-bind (err-code width height)
      (with-dynamic-foreign-objects ((width (:pointer :short))
                                     (height (:pointer :short)))
        (values (fm-binary-data-get-sizedata self width height)
                (dereference width)
                (dereference height)))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_BinaryData_GetSIZEData."
             err-code))
    (values width height)))

(defun fm-binary-data-add-size-data* (self width height)
  "Like FM-BINARY-DATA-ADD-SIZE-DATE but with error checking."
  (let ((err-code (fm-binary-data-add-sizedata self width height)))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_BinaryData_AddSIZEData."
             err-code))))

(defmacro with-binary-data ((ptr) &body body)
  "Executes BODY with PTR bound to a fresh BinaryData object.
The object is guaranteed to be deleted after the execution of
BODY."
  `(let (,ptr)
     (unwind-protect
         (progn
           (setq ,ptr (fm-binary-data-constructor1))
           ,@body)
       (when ,ptr
         (ignore-errors
           (fm-binary-data-delete ,ptr))))))

(defclass binary-data-object (fm-object)
  ()
  (:documentation "A BINARY-DATA-OBJECT is a Lisp object which is
a proxy for a FileMaker `BinaryData' object."))

(defmethod fm-delete ((binary-data-object binary-data-object))
  "Deletes the BinaryData \(C++) object which is proxied by
BINARY-DATA-OBJECT."
  (fm-binary-data-delete (pointer binary-data-object)))

(defun make-binary-data-object ()
  "Creates and returns a new BINARY-DATA-OBJECT."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  (when ptr
                    (ignore-errors
                      (fm-binary-data-delete ptr))))))
      (setq ptr (fm-binary-data-constructor1))
      (make-instance 'binary-data-object :pointer ptr))))

(defmethod get-count ((binary-data-object binary-data-object))
  "Returns the number of streams of BINARY-DATA-OBJECT."
  (fm-binary-data-get-count (pointer binary-data-object)))

(defmethod get-index ((binary-data-object binary-data-object) data-type)
  "Returns the index of the stream corresponding to the data type
DATA-TYPE \(a four-character Lisp string) of BINARY-DATA-OBJECT."
  (let ((index
         (with-quadchar (quadchar-ptr data-type)
           (fm-binary-data-get-index (pointer binary-data-object) quadchar-ptr))))
    (cond ((minusp index) nil)
          (t index))))

(defmethod get-total-size ((binary-data-object binary-data-object))
  "Returns the total size of BINARY-DATA-OBJECT."
  (fm-binary-data-get-total-size (pointer binary-data-object)))

(defmethod get-type ((binary-data-object binary-data-object) index)
  "Returns the data type \(as a Lisp string) of the stream with
index INDEX of BINARY-DATA-OBJECT."
  (with-quadchar (quadchar-ptr)
    (fm-binary-data-get-type (pointer binary-data-object) index quadchar-ptr)
    (quadchar-to-string quadchar-ptr)))

(defmethod get-size ((binary-data-object binary-data-object) index)
  "Returns the size of the stream with index INDEX of
BINARY-DATA-OBJECT."
  (fm-binary-data-get-size (pointer binary-data-object) index))

(defmethod get-data ((binary-data-object binary-data-object) index
                     &key (offset 0)
                          (amount (- (get-size binary-data-object index) offset))
                          (result (make-array amount
                                              :element-type '(unsigned-byte 8)
                                              :allocation :static)
                                  result-provided-p)
                          (start 0))
  "Returns AMOUNT octets of the contents of the stream with index
INDEX of BINARY-DATA-OBJECT beginning at octet OFFSET.  If RESULT
is provided, it must be a Lisp vector of element
type \(UNSIGNED-BYTE 8) with static allocation which will be
filled with the corresponding data and returned.  If RESULT is
not provided, a large enough vector will be created.  If RESULT is
provided, the vector will be filled beginning from position START.
If RESULT is not provided, START must not be provided."
  (unless (and (vectorp result)
               (subtypep (array-element-type result) '(unsigned-byte 8)))
    (error "Argument ~S should have been a vector of octets."
           result))
  (unless (>= (- (length result) start) amount)
    (error "Argument ~S is not big enough to hold ~A octets of data."
           result amount))
  (unless (sys:staticp result)
    (error "Argument ~S must have been allocated statically."
           result))
  (with-dynamic-lisp-array-pointer (arr-ptr result :start (if result-provided-p start 0))
    (fm-binary-data-get-data* (pointer binary-data-object) index
                              offset amount arr-ptr))
  result)

(defmethod add-data ((binary-data-object binary-data-object) data-type data
                     &key (start 0) (end (length data)))
  "Adds data from the Lisp vector \(of element type
\(UNSIGNED-BYTE 8)) DATA to the stream of data type DATA-TYPE \(a
four-character Lisp string) of BINARY-DATA-OBJECT.  If START
and/or END are provided, only the vector data from START to END
is used.  For large vectors, the operation is likely to be a tad
faster if DATA was allocated statically."
  (unless (and (vectorp data)
               (subtypep (array-element-type data) '(unsigned-byte 8)))
    (error "Argument ~S should have been a vector of octets."
           data))
  (unless (sys:staticp data)
    (let ((static-data (make-array (- end start)
                                   :element-type '(unsigned-byte 8)
                                   :allocation :static)))
      (replace static-data data :start2 start :end2 end)
      (setq data static-data
            end (- end start)
            start 0)))
  (with-dynamic-lisp-array-pointer (arr-ptr data :start start)
    (with-quadchar (quadchar-ptr data-type)
      (fm-binary-data-add* (pointer binary-data-object)
                           quadchar-ptr (- end start) arr-ptr)))
  binary-data-object)

(defmethod remove-data ((binary-data-object binary-data-object) data-type)
  "Removes the stream of data type DATA-TYPE \(a four-character
Lisp string) from BINARY-DATA."
  (with-quadchar (quadchar-ptr data-type)
    (fm-binary-data-remove (pointer binary-data-object) quadchar-ptr))
  binary-data-object)

(defmethod remove-all ((binary-data-object binary-data-object))
  "Removes all streams from BINARY-DATA-OBJECT."
  (fm-binary-data-remove-all (pointer binary-data-object))
  binary-data-object)

(defmethod get-fnam-data ((binary-data-object binary-data-object) &optional as-text-p)
  "Returns the filename of the filename \(FNAM) stream of
BINARY-DATA-OBJECT.  If AS-TEXT-P is true, the result will be a
TEXT-OBJECT."
  (cond (as-text-p
         (let ((result (make-text-object)))
           (fm-binary-data-get-fnam-data* (pointer binary-data-object) (pointer result))
           result))
        (t
         (with-text (text-ptr)
           (fm-binary-data-get-fnam-data* (pointer binary-data-object) text-ptr)
           (fm-text-get-string text-ptr)))))

(defmethod add-fnam-data ((binary-data-object binary-data-object) (file-path-list text-object))
  "Sets the filename of the filename \(FNAM) stream of
BINARY-DATA to FILE-PATH-LIST."
  (fm-binary-data-add-fnam-data* (pointer binary-data-object) (pointer file-path-list))
  binary-data-object)

(defmethod add-fnam-data ((binary-data-object binary-data-object) (file-path-list string))
  "Sets the filename of the filename \(FNAM) stream of
BINARY-DATA to FILE-PATH-LIST."
  (with-text (text-ptr file-path-list)
    (fm-binary-data-add-fnam-data* (pointer binary-data-object) text-ptr))
  binary-data-object)

(defmethod get-size-data ((binary-data-object binary-data-object))
  "Returns as two values the width and height of
BINARY-DATA-OBJECT if it contains an image."
  (fm-binary-data-get-size-data* (pointer binary-data-object)))

(defmethod add-size-data ((binary-data-object binary-data-object) width height)
  "Sets the width and height of the image in BINARY-DATA-OBJECT."
  (fm-binary-data-add-size-data* (pointer binary-data-object) width height)
  binary-data-object)
