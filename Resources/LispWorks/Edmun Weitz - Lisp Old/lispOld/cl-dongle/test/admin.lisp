;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/test/admin.lisp,v 1.1 2008/04/27 00:57:59 edi Exp $

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

(in-package :cl-dongle-test)

(defun authentication-test ()
  (when *test-with-dongle-p*
    (with-test ("Testing authentication.")
      (let ((wrong-code (loop repeat 12 collect (random-u32))))
        (unless (sequence-equal wrong-code *test-authentication-code*)
          (with-expected-error (authentication-failed)
            (authenticate wrong-code))))
      (authenticate *test-authentication-code*))))

(defun dongle-presence-test ()
  (when *test-with-dongle-p*
    (with-test ("Checking the dongle's presence.")
      (check (dongle-present-p))
      (assert-dongle)
      ;; this and other parts of the tests obviously assume that
      ;; there's only one dongle plugged in while the tests are
      ;; running...
      (let ((*product-id* (mod (1+ *product-id*) #xffff)))
        (check (not (dongle-present-p)))
        (with-expected-error (dongle-not-found)
          (assert-dongle))))))
  
(defun product-id-test ()
  (when *test-with-dongle-p*
    (with-test ("Checking and temporarily modifying the dongle's product ID.")
      (check (= *product-id* (product-id)))
      (let ((new-product-id (mod (1+ *product-id*) #xffff)))
        (setf (product-id) new-product-id)
        (check (= new-product-id (product-id)))
        (check (dongle-present-p new-product-id))
        (check (not (dongle-present-p)))
        (setf (product-id new-product-id) *product-id*))
      (check (= *product-id* (product-id))))))

(defun dongle-info-test ()
  (when *test-with-dongle-p*
    (with-test ("Retrieving information about the dongle.")
      (format *error-output* "The dongle with the product ID ~A is a ~A~A dongle.~%"
              *product-id*
              (ecase (dongle-interface)
                (:usb #\U)
                (:lpt #\L))
              (dongle-type))
      (format *error-output* "The software version is ~{~A~^.~}.~%"
              (dongle-software-version))
      (format *error-output* "The hardware version is ~{~A~^.~}.~%"
              (dongle-hardware-version))
      (format *error-output* "The memory size is ~A octets.~%"
              (* 4 (dongle-memory-size)))
      (format *error-output* "It has ~A programmable counters and ~A keys.~%"
              (dongle-number-of-counters)
              (dongle-number-of-keys)))))
              
(defun counter-test (counter-number)
  (with-test ((format nil "Testing reading and writing of counter number ~A." counter-number))
    (let ((old-value (counter-value counter-number))
          (new-value (random-u32)))
      (setf (counter-value counter-number) new-value)
      (check (= new-value (counter-value counter-number)))
      (setf (counter-value counter-number) old-value)
      (check (= old-value (counter-value counter-number))))))

(defun counter-tests (&optional (n 10))
  (when *test-with-dongle-p*
    (let ((number-of-counters (dongle-number-of-counters)))
      (when (plusp number-of-counters)
        (loop repeat n
              for counter-number = (random number-of-counters)
              do (counter-test counter-number))))))
        
    
  