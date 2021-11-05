;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/conditions.lisp,v 1.5 2008/04/18 07:53:29 edi Exp $

;;; Copyright (c) 2008, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :cl-dongle)

(define-condition dongle-condition (condition)
  ()
  (:documentation "The superclass for all conditions signalled by the
CL-DONGLE library."))

(define-condition dongle-error (dongle-condition error)
  ()
  (:documentation "The superclass for all errors signalled by the
CL-DONGLE library."))

(define-condition dongle-not-found (dongle-error)
  ()
  (:report "No matching dongle found.")
  (:documentation "This error is signalled if no dongle corresponding
to the product ID which was provided was found.  This is equivalent to
getting the return value SGL_DGL_NOT_FOUND from the SG-Lock API."))

(define-condition lpt-port-busy (dongle-error)
  ()
  (:report "At least one LPT port is busy.")
  (:documentation "This error is signalled if at least one LPT port is
busy and no matching dongles were found on the other ports, if there
are any.  This is equivalent to getting the return value SGL_LPT_BUSY
from the SG-Lock API."))

(define-condition usb-port-busy (dongle-error)
  ()
  (:report "At least one USB port is busy.")
  (:documentation "This error is signalled if at least one USB port is
busy and no matching dongles were found on the other ports, if there
are any.  This is equivalent to getting the return value SGL_USB_BUSY
from the SG-Lock API."))

(define-condition lpt-open-error (dongle-error)
  ()
  (:report "SG-Lock LPT driver not found.")
  (:documentation "This error is signalled if the SG-Lock LPT driver
wasn't found although LPT support was installed.  This is equivalent
to getting the return value SGL_LPT_OPEN_ERROR from the SG-Lock
API."))

(define-condition no-lpt-port-found (dongle-error)
  ()
  (:report "No LPT port found.")
  (:documentation "This error is signalled if no LPT port was found on
the PC although LPT support was installed.  This is equivalent to
getting the return value SGL_NO_LPT_PORT_FOUND from the SG-Lock
API."))

(define-condition authentication-required (dongle-error)
  ()
  (:report "Authentication is required before other functions can be called.")
  (:documentation "This error is signalled if functions of the SG-Lock
API are called prior to the mandatory initial authentication - see the
function AUTHENTICATE.  This is equivalent to getting the return value
SGL_AUTHENTICATION_REQUIRED from the SG-Lock API."))

(define-condition authentication-failed (dongle-error)
  ()
  (:report "Authentication failed.")
  (:documentation "This error is signalled if the mandatory initial
authentication \(function AUTHENTICATE) failed.  This is equivalent to
getting the return value SGL_AUTHENTICATION_FAILED from the SG-Lock
API."))

(define-condition function-not-supported (dongle-error)
  ()
  (:report "Function not supported by this dongle.")
  (:documentation "This error is signalled if an API function was
called which is not supported by the current dongle.  This is
equivalent to getting the return value SGL_FUNCTION_NOT_SUPPORTED from
the SG-Lock API."))

(define-condition parameter-invalid (dongle-error)
  ()
  (:report "Invalid parameter.")
  (:documentation "This error is signalled if an API function was
called with parameters which are out of range.  This is equivalent to
getting the return value SGL_PARAMETER_INVALID from the SG-Lock
API."))

(define-condition signature-invalid (dongle-error)
  ()
  (:report "Invalid signature.")
  (:documentation "This error is signalled if one of the API function
was asked to verify a signature which turned out to be invalid.  This
is equivalent to getting the return value SGL_SIGNATURE_INVALID from
the SG-Lock API."))

(define-condition unknown-return-value (dongle-error)
  ((return-value :initarg :return-value
                 :reader return-value))
  (:report (lambda (condition stream)
             (format stream "Unknown return value ~S from SG-Lock API."
                     (return-value condition))))
  (:documentation "This is a typical \"this should not happen\" error.
It will be signalled in the case that one of the C functions in the
SG-Lock DLL returns an undocumented value."))

(defun dongle-error (return-value)
  "Utility function to signal a DONGLE-ERROR depending on the return
value of one of the C functions."
  (let ((error-class (case return-value
                       (#.+sgl-dgl-not-found+ 'dongle-not-found)
                       (#.+sgl-lpt-busy+ 'lpt-port-busy)
                       (#.+sgl-lpt-open-error+ 'lpt-open-error)
                       (#.+sgl-no-lpt-port-found+ 'no-lpt-port-found)
                       (#.+sgl-authentication-required+ 'authentication-required)
                       (#.+sgl-authentication-failed+ 'authentication-failed)
                       (#.+sgl-function-not-supported+ 'function-not-supported)
                       (#.+sgl-parameter-invalid+ 'parameter-invalid)
                       (#.+sgl-signature-invalid+ 'signature-invalid)
                       (#.+sgl-usb-busy+ 'usb-port-busy))))
    (cond (error-class (error error-class))
          (t (error 'unknown-return-value :return-value return-value)))))

(defun check-return-value (return-value)
  "Checks the return value of an SG-Lock C function and signals an
error if necessary."
  (unless (zerop return-value)
    (dongle-error return-value)))

(define-condition simple-dongle-error (dongle-error simple-error)
  ()
  (:documentation "Used to signal errors not coming from the C API."))

(defun simple-dongle-error (format-control &rest format-arguments)
  "Helper function to signal errors of type SIMPLE-DONGLE-ERROR."
  (error 'simple-dongle-error
         :format-control format-control
         :format-arguments format-arguments))