;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/style-objects.lisp,v 1.9 2010/07/22 09:38:06 edi Exp $

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

(defclass style-object (fm-object)
  ()
  (:documentation "A STYLE-OBJECT is a Lisp object which is a proxy
for a FileMaker `CharacterStyle' object."))

(defmethod fm-delete ((style-object style-object))
  "Deletes the CharacterStyle \(C++) object which is proxied by STYLE-OBJECT."
  (fm-character-style-delete (pointer style-object)))

(defun make-style-object (&key font face size color)
  "Creates and returns a STYLE-OBJECT and sets and enables FONT,
FACE, SIZE, and COLOR if provided."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  (when ptr
                    (ignore-errors
                      (fm-character-style-delete ptr))))))
      (setq ptr (fm-character-style-constructor1))
      (when font
        (fm-character-style-enable-font ptr)
        (fm-character-style-set-font ptr font))
      (when face
        (fm-character-style-enable-face ptr face)
        (fm-character-style-set-face ptr face))
      (when size
        (fm-character-style-enable-size ptr)
        (fm-character-style-set-size ptr size))
      (when color
        (fm-character-style-enable-color ptr)
        (fm-character-style-set-color (pointer color) font))
      (make-instance 'style-object :pointer ptr))))

(defmethod font-enabled-p ((style-object style-object))
  "Checks whether the font property of STYLE-OBJECT is enabled."
  (fm-character-style-is-font-enabled (pointer style-object)))

(defmethod (setf font-enabled-p) (new-value (style-object style-object))
  "Enables or disables the font property of STYLE-OBJECT.
NEW-VALUE is treated as a generalized boolean."
  (cond (new-value
         (fm-character-style-enable-font (pointer style-object)))
        (t
         (fm-character-style-disable-font (pointer style-object))))
  new-value)

(defmethod face-enabled-p ((style-object style-object) face)
  "Checks whether the face property of STYLE-OBJECT corresponding to FACE is enabled."
  (fm-character-style-is-face-enabled (pointer style-object) face))

(defmethod (setf face-enabled-p) (new-value (style-object style-object) face)
  "Enables or disables the face property of STYLE-OBJECT corresponding to FACE.
NEW-VALUE is treated as a generalized boolean."
  (cond (new-value
         (fm-character-style-enable-face (pointer style-object) face))
        (t
         (fm-character-style-disable-face (pointer style-object) face)))
  new-value)

(defmethod any-face-enabled-p ((style-object style-object))
  "Checks whether any face of STYLE-OBJECT is enabled."
  (fm-character-style-is-any-face-enabled (pointer style-object)))

(defmethod disable-all-faces ((style-object style-object))
  "Disables all faces of STYLE-OBJECT."
  (fm-character-style-disable-all-faces (pointer style-object))
  (values))

(defmethod size-enabled-p ((style-object style-object))
  "Checks whether the size property of STYLE-OBJECT is enabled."
  (fm-character-style-is-size-enabled (pointer style-object)))

(defmethod (setf size-enabled-p) (new-value (style-object style-object))
  "Enables or disables the size property of STYLE-OBJECT.
NEW-VALUE is treated as a generalized boolean."
  (cond (new-value
         (fm-character-style-enable-size (pointer style-object)))
        (t
         (fm-character-style-disable-size (pointer style-object))))
  new-value)

(defmethod color-enabled-p ((style-object style-object))
  "Checks whether the color property of STYLE-OBJECT is enabled."
  (fm-character-style-is-color-enabled (pointer style-object)))

(defmethod (setf color-enabled-p) (new-value (style-object style-object))
  "Enables or disables the color property of STYLE-OBJECT.
NEW-VALUE is treated as a generalized boolean."
  (cond (new-value
         (fm-character-style-enable-color (pointer style-object)))
        (t
         (fm-character-style-disable-color (pointer style-object))))
  new-value)

(defmethod disable-all ((style-object style-object))
  "Disables all properties of STYLE-OBJECT."
  (fm-character-style-disable-all (pointer style-object))
  (values))

(defmethod font ((style-object style-object))
  "Returns the font ID of STYLE-OBJECT."
  (fm-character-style-get-font (pointer style-object)))

(defmethod (setf font) (new-value (style-object style-object))
  "Sets the font ID of STYLE-OBJECT."
  (fm-character-style-set-font (pointer style-object) new-value)
  new-value)

(defmethod face ((style-object style-object))
  "Returns the face of STYLE-OBJECT."
  (fm-character-style-get-face (pointer style-object)))

(defmethod (setf face) (new-value (style-object style-object))
  "Sets the face of STYLE-OBJECT."
  (fm-character-style-set-face (pointer style-object) new-value)
  new-value)

(defmethod size ((style-object style-object))
  "Returns the size of STYLE-OBJECT."
  (fm-character-style-get-size (pointer style-object)))

(defmethod (setf size) (new-value (style-object style-object))
  "Sets the size of STYLE-OBJECT."
  (fm-character-style-set-size (pointer style-object) new-value)
  new-value)

(defmethod color ((style-object style-object))
  "Returns the color of STYLE-OBJECT as a COLOR-OBJECT."
  (make-instance 'color-object
                 :pointer (fm-character-style-get-color (pointer style-object))
                 :do-not-delete t))

(defmethod (setf color) ((new-value color-object) (style-object style-object))
  "Sets the color of STYLE-OBJECT.  NEW-VALUE should be a COLOR-OBJECT."
  (fm-character-style-set-color (pointer style-object) (pointer new-value))
  new-value)
