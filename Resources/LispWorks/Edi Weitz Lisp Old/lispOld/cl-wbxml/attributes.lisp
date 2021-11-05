;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/attributes.lisp,v 1.5 2007/01/01 23:44:09 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :cl-wbxml)

(defclass attribute ()
  ((local-name :initarg :local-name
               :reader attribute-local-name
               :documentation "The local name of the attribute.")
   (namespace-uri :initarg :namespace-uri
                  :reader attribute-namespace-uri
                  :documentation "The namespace URI of the
attribute or NIL.")
   (qname :initarg :qname
          :initform nil
          :reader attribute-qname
          :documentation "The qualified name of the attribute.
This might be NIL if the attribute name came from an attribute
start token.")
   (value :initarg :value
          :initform nil
          :reader attribute-value
          :documentation "The attribute's value - a string or
NIL."))
  (:documentation "Objects of this class are used to represent
XML attributes."))
               
(defmethod print-object ((attribute attribute) stream)
  "A custome method to print ATTRIBUTE objects."
  (print-unreadable-object (attribute stream :type t)
    (princ (or (attribute-local-name attribute)
               (attribute-qname attribute)
               "<UNNAMED>"))))