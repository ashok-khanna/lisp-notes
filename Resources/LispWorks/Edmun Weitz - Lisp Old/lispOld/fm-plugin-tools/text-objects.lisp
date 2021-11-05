;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/text-objects.lisp,v 1.18 2010/07/22 09:38:06 edi Exp $

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

#+:macosx
(defun fm-text-assign-unicode (self string)
  "This is a wrapper around FM-TEXT-ASSIGN-UNICODE% which sets up its
second argument by converting the Lisp string STRING to an array of
16-bit values representing a Unicode C string.

Only needed on Mac OS X as a workaround because :EF-WC-STRING can't be
used."
  (let ((length (length string)))
    ;; convert the string
    (with-dynamic-foreign-objects ((ptr :unsigned-short :nelems (1+ length)))
      ;; use Unicode code points
      (dotimes (index length)
        (setf (dereference ptr :index index)
              (char-code (char string index))))
      ;; delimiter for C string
      (setf (dereference ptr :index length) 0)
      ;; call the "real" function
      (fm-text-assign-unicode% self ptr))))

(defun fm-text-get-string (self &key (position 0) size)
  "Returns, as a Lisp string, the substring of length SIZE
beginning at POSITION from the string represented by the
FileMaker Text object at SELF."
  (when (or (null size)
            (= size +k-size-end+))
    (setq size (fm-text-get-size self)))
  (convert-line-endings 
   (with-dynamic-foreign-objects ()
     (let ((buffer (allocate-dynamic-foreign-object :type #+:win32 :wchar-t #-:win32 :unsigned-short
                                                    :nelems (1+ size))))
       (fm-text-get-unicode self buffer position size)
       (setf (dereference buffer :index size) #\Null)
       #+:win32
       (convert-from-foreign-string buffer :external-format :unicode)
       #-:win32
       (loop with string = (make-string size :element-type 'lw:simple-char)
             for index from 0 below size
             do (setf (char string index)
                      (code-char (dereference buffer :index index)))
             finally (return string))))))

(defmacro with-text ((ptr &optional string) &body body)
  "Executes BODY with PTR bound to a fresh Text object optionally
intialized to the Lisp string STRING.  The object is guaranteed
to be deleted after the execution of BODY."
  (rebinding (string)
    `(let (,ptr)
       (unwind-protect
           (progn
             (setq ,ptr (fm-text-constructor1))
             (when ,string
               (fm-text-assign-unicode ,ptr ,string))
             ,@body)
         (when ,ptr
           (ignore-errors
             (fm-text-delete ,ptr)))))))

(defclass text-object (fm-object)
  ()
  (:documentation "A TEXT-OBJECT is a Lisp object which is a
proxy for a FileMaker `Text' object."))

(defmethod fm-delete ((text-object text-object))
  "Deletes the Text \(C++) object which is proxied by
TEXT-OBJECT."
  (fm-text-delete (pointer text-object)))

(defun make-text-object (&optional (string ""))
  "Creates and returns a TEXT-OBJECT representing the string
STRING."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  (when ptr
                    (ignore-errors
                      (fm-text-delete ptr))))))
      (setq ptr (fm-text-constructor1))
      (fm-text-assign-unicode ptr string)
      (make-instance 'text-object :pointer ptr))))

(defmethod size ((text-object text-object))
  "Returns the size of TEXT-OBJECT."
  (fm-text-get-size (pointer text-object)))

(defmethod as-string ((text-object text-object) &key (position 0) size)
  "Returns the string represented by TEXT-OBJECT as a Lisp
string.  Optionally you can return only the SIZE characters
beginning at POSITION."
  (fm-text-get-string (pointer text-object)
                      :position position
                      :size size))

(defmethod (setf as-string) ((new-value string) (text-object text-object) &key)
  "Sets the string represented by TEXT-OBJECT to NEW-VALUE, a
Lisp string."
  (fm-text-assign-unicode (pointer text-object) new-value)
  new-value)

(defmethod set-text ((text-object text-object) (other-text-object text-object)
                     &key (position 0) (size +k-size-end+))
  "Sets TEXT-OBJECT to be the part of size SIZE starting at
position POSITION of OTHER-TEXT-OBJECT."
  (fm-text-set-text (pointer text-object) (pointer other-text-object) position size)
  text-object)

(defmethod append-text ((text-object text-object) (other-text-object text-object)
                        &key (position 0) (size +k-size-end+))
  "Appends to the end of TEXT-OBJECT the part of size SIZE
starting at position POSITION of OTHER-TEXT-OBJECT."
  (fm-text-append-text (pointer text-object) (pointer other-text-object) position size)
  text-object)

(defmethod insert-text ((text-object text-object) (other-text-object text-object)
                        &key (position 0))
  "Inserts OTHER-TEXT-OBJECT at position POSITION of TEXT-OBJECT."
  (fm-text-insert-text (pointer text-object) (pointer other-text-object) position)
  text-object)

(defmethod delete-text ((text-object text-object) position
                        &key (size +k-size-end+))
  "Deletes the part of TEXT-OBJECT of size SIZE beginning at
position POSITION."
  (fm-text-delete-text (pointer text-object) position size)
  text-object)

(defmethod get-style ((text-object text-object) position)
  "Returns the character style \(as a STYLE-OBJECT) of the
character at position POSITION of TEXT-OBJECT."
  (let ((style-object (make-style-object)))
    (fm-text-get-style (pointer text-object) (pointer style-object) position)
    style-object))

(defmethod get-default-style ((text-object text-object))
  "Returns the default character style \(as a STYLE-OBJECT) of
TEXT-OBJECT."
  (let ((style-object (make-style-object)))
    (fm-text-get-default-style (pointer text-object) (pointer style-object))
    style-object))

(defmethod set-style ((text-object text-object) (style-object style-object)
                      position size)
  "Sets the character style of the SIZE characters of TEXT-OBJECT
beginning at position POSITION to STYLE-OBJECT."
  (fm-text-set-style (pointer text-object) (pointer style-object) position size)
  text-object)

(defmethod remove-style ((text-object text-object) (style-object style-object))
  "Removes the character style STYLE-OBJECT from TEXT-OBJECT."
  (fm-text-remove-style (pointer text-object) (pointer style-object))
  text-object)

(defmethod reset-all-style-buffers ((text-object text-object))
  "Removes all characters styles from TEXT-OBJECT."
  (fm-text-reset-all-style-buffers (pointer text-object))
  text-object)