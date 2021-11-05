;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/handler.lisp,v 1.13 2007/01/01 23:44:09 edi Exp $

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

(defgeneric start-document (handler)
  (:documentation "This generic function is called once when the
parser starts parsing a document.")
  (:method (handler)
    (declare (ignore handler))))

(defgeneric end-document (handler)
  (:documentation "This generic function is called once when the
parser is done parsing a document.")
  (:method (handler)
    (declare (ignore handler))))

(defgeneric start-element (handler namespace-uri local-name qname attributes)
  (:documentation "This generic function is called at the start
of each element.  LOCAL-NAME is the name of the element and
NAMESPACE-URI the corresponding namespace URI \(or NIL).  QNAME
is the qualified name of the element but can also be NIL if the
name came from a tag token.  ATTRIBUTES is a list of ATTRIBUTE
objects representing the element's attributes.")
  (:method (handler namespace-uri local-name qname attributes)
   (declare (ignore handler namespace-uri local-name qname attributes))))

(defgeneric end-element (handler namespace-uri local-name qname)
  (:documentation "This generic function is called at the end of
each element.  LOCAL-NAME is the name of the element and
NAMESPACE-URI the corresponding namespace URI \(or NIL).  QNAME
is the qualified name of the element but can also be NIL if the
name came from a tag token.")
  (:method (handler namespace-uri local-name qname)
   (declare (ignore handler namespace-uri local-name qname))))

(defgeneric processing-instruction (handler target data)
  (:documentation "This generic function is called once for each
processing instruction.  TARGET and DATA are both strings, DATA
can also be NIL.")
  (:method (handler target data)
   (declare (ignore handler target data))))

(defgeneric characters (handler data)
  (:documentation "This generic function is called once or
several times for the character data within the contents of an
element.  DATA will usually be a string but it can also be a list
of octets \(if the OPAQUE token was encountered) or NIL (if
*EXTENSION-FUNCTION* declined to handle an extension token.)")
  (:method (handler data)
   (declare (ignore handler data))))

(defclass xmls-handler ()
  ((element-stack :initform nil
                  :accessor element-stack
                  :documentation "The stack of elements which
were opened and not closed yet.")
   (root :initform nil
         :accessor root
         :documentation "The root element."))
  (:documentation "Objects of this class can be fed to the parser
to create output similar to that of XMLS.
See <http://common-lisp.net/project/xmls/>."))

(defun make-xmls-handler ()
  "Returns a handler which can be used to generate XMLS-like
output."
  (make-instance 'xmls-handler))

(defmethod end-document ((handler xmls-handler))
  "If done return the root element."
  (root handler))

(defmethod start-element ((handler xmls-handler) namespace-uri local-name qname attributes)
  "Creates a node without children and adds it to the children of
its parent \(if there is one)."
  (declare (ignore qname))
  ;; get parent (if available) from top of element stack
  (let ((parent (car (element-stack handler)))
        ;; create node
        (node (list (cond (namespace-uri (cons local-name namespace-uri))
                          (t local-name))
                    (loop for attribute in attributes
                          collect (with-accessors ((namespace-uri attribute-namespace-uri)
                                                   (local-name attribute-local-name)
                                                   (value attribute-value))
                                      attribute
                                    (list (cond (namespace-uri (cons local-name namespace-uri))
                                                (t local-name))
                                          value))))))
    ;; add to parent's children or otherwise set as root element
    (cond (parent (push node (xmls-children parent)))
          (t (setf (root handler) node)))
    ;; push on stack
    (push node (element-stack handler))))

(defmethod end-element ((handler xmls-handler) namespace-uri local-name qname)
  "Finishes the node which was created in the corresponding call
to START-ELEMENT."
  (declare (ignore namespace-uri local-name qname))
  (let ((node (pop (element-stack handler))))
    ;; reverse the list of children because they were PUSHed onto the
    ;; list
    (setf (xmls-children node)
          (reverse (xmls-children node)))))

(defmethod characters ((handler xmls-handler) data)
  "Add DATA to list of node children.  Concatenate with previous
string if possible."
  (let* ((parent (car (element-stack handler)))
         (previous (car (xmls-children parent))))
    (cond ((null data))
          ((and (typep data 'string)
                (typep previous 'string))
           (setf (car (xmls-children parent))
                 (concatenate 'string previous data)))
          (t (push data (xmls-children parent))))))
