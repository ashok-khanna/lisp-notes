;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/color-objects.lisp,v 1.5 2010/07/22 09:38:05 edi Exp $

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

(defclass color-object (fm-object)
  ()
  (:documentation "A COLOR-OBJECT is a Lisp object which is a proxy
for a FileMaker `Color' object."))

(defmethod fm-delete ((color-object color-object))
  "Deletes the Color \(C++) object which is proxied by COLOR-OBJECT."
  (fm-color-delete (pointer color-object)))

(defun make-color-object (&key (red 255) (green 255) (blue 255) (alpha 255))
  "Creates and returns a COLOR-OBJECT with color channels
corresponding to RED, GREEN, BLUE, and ALPHA."
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  ;; delete object if something bad happens during
                  ;; creation
                  (when ptr
                    (ignore-errors
                      (fm-color-delete ptr))))))
      (setq ptr (fm-color-constructor2 red green blue alpha))
      (make-instance 'color-object :pointer ptr))))

(defmethod red ((color-object color-object))
  "Returns the red component of COLOR-OBJECT."
  (fm-color-get-red (pointer color-object)))

(defmethod (setf red) (new-value (color-object color-object))
  "Sets the red component of COLOR-OBJECT to RED."
  (fm-color-set-red (pointer color-object) new-value)
  new-value)

(defmethod green ((color-object color-object))
  "Returns the green component of COLOR-OBJECT."
  (fm-color-get-green (pointer color-object)))

(defmethod (setf green) (new-value (color-object color-object))
  "Sets the green component of COLOR-OBJECT to GREEN."
  (fm-color-set-green (pointer color-object) new-value)
  new-value)

(defmethod blue ((color-object color-object))
  "Returns the blue component of COLOR-OBJECT."
  (fm-color-get-blue (pointer color-object)))

(defmethod (setf blue) (new-value (color-object color-object))
  "Sets the blue component of COLOR-OBJECT to BLUE."
  (fm-color-set-blue (pointer color-object) new-value)
  new-value)

(defmethod alpha ((color-object color-object))
  "Returns the alpha component of COLOR-OBJECT."
  (fm-color-get-alpha (pointer color-object)))

(defmethod (setf alpha) (new-value (color-object color-object))
  "Sets the alpha component of COLOR-OBJECT to ALPHA."
  (fm-color-set-alpha (pointer color-object) new-value)
  new-value)
