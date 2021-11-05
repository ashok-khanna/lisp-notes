;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/conditions.lisp,v 1.6 2013/01/11 17:14:20 edi Exp $

;;; Copyright (c) 2007-2013, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :odd-streams)

(define-condition odd-stream-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to odd streams."))

(define-condition odd-stream-error (odd-stream-condition stream-error)
  ()
  (:documentation "Superclass for all errors related to odd streams."))

(define-condition odd-stream-simple-error (odd-stream-error simple-condition)
  ()
  (:documentation "Like ODD-STREAM-ERROR but with formatting capabilities."))

(define-condition odd-stream-position-spec-error (odd-stream-simple-error)
  ((position-spec :initarg :position-spec
                  :reader odd-stream-position-spec-error-position-spec))
  (:report (lambda (condition stream)
             (format stream "Unknown file position designator: ~S."
                     (odd-stream-position-spec-error-position-spec condition))))
  (:documentation "Errors of this type are signaled if an erroneous
position spec is used in conjunction with FILE-POSITION."))

(defun odd-stream-error (format-control &rest format-arguments)
  "Signals an error of type ODD-STREAM-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'odd-stream-simple-error
         :stream nil
         :format-control format-control
         :format-arguments format-arguments))

(define-condition odd-stream-warning (odd-stream-condition warning)
  ()
  (:documentation "Superclass for all warnings related to odd streams."))

(define-condition odd-stream-operation-not-implemented (odd-stream-warning)
  ((operation :initarg :operation
              :reader odd-stream-operation-not-implemented-operation)
   (stream :initarg :stream
           :reader odd-stream-operation-not-implemented-stream))
  (:report (lambda (condition stream)
             (format stream "The operation ~S is not implemented for the stream ~S."
                     (odd-stream-operation-not-implemented-operation condition)
                     (odd-stream-operation-not-implemented-stream condition))))
  (:documentation "This warning is signalled when an operation \(like
CLEAR-INPUT for example) is not implemented for an odd stream."))

