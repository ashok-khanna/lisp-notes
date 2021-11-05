;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/test/store.lisp,v 1.2 2008/04/27 14:55:34 edi Exp $

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

(defun store-data-test (size &optional (n 5))
  (with-test ("Storing and retrieving random bits of data.")
    (dotimes (i n)
      (let* ((length (1+ (random size)))
             (address (random (1+ (- size length))))
             (data (loop repeat length collect (random-u32))))
        (format *error-output* "~A dwords at address ~A.~%"
                length address)
        (store-data address data)
        (check (sequence-equal data (retrieve-data address length)))
        (store-data address (coerce data 'vector))
        (check (sequence-equal data (retrieve-data address length)))
        (let ((part-length (random length)))
          (when (plusp part-length)
            (check (sequence-equal (subseq data 0 part-length)
                                   (retrieve-data address part-length)))
            (check (sequence-equal (subseq data part-length)
                                   (retrieve-data (+ address part-length) (- length part-length))))))))))

(defun store-string-test (&optional (n 10))
  (with-test ((format nil "Storing and retrieving ~R random string~:P." n))
    (dotimes (i n)
      (let ((string (random-string 50))
            (address (random 10)))
        (store-string address string)
        (check (string= (retrieve-string address) string))))))

(defun store-object-test (&optional (n 10))
  (with-test ("Storing and retrieving a few random Lisp objects.")
    (format *error-output* "~:(~R~) floating point numbers.~%" (* n 3))
    (dolist (prototype '(1f10 1s10 1d10))
      (dotimes (i n)
        (let* ((factor (expt 10 (- 10 (random 20))))
               (float (* (if (plusp (random 2)) 1 -1)
                         factor
                         (random prototype)))
               (address (random 10)))
          (store-lisp-object address float)
          (check (eql (retrieve-lisp-object address) float)))))
    (format *error-output* "~:(~R~) integers.~%" n)
    (dotimes (i n)
      (let ((integer (* (if (plusp (random 2)) 1 -1)
                        (random #.(* 2 most-positive-fixnum))))
            (address (random 10)))
        (store-lisp-object address integer)
        (check (eql (retrieve-lisp-object address) integer))))
    (format *error-output* "~:(~R~) symbols.~%" n)
    (dotimes (i n)
      (let* ((packages (list-all-packages))
             (package (nth (random (length packages)) packages))
             (symbols (loop for symbol being the symbols of package collect symbol))
             (symbol (and symbols (nth (random (length symbols)) symbols)))
             (address (random 10)))
        (store-lisp-object address symbol)
        (check (eq (retrieve-lisp-object address) symbol))))))

(defun storage-tests ()
  (when *test-with-dongle-p*
    (let ((size (dongle-memory-size)))
      (when (plusp size)
        (format *error-output* "Backing up ~A octets of data.~%"
                (* 4 size))
        (let ((backup (retrieve-data 0 size)))
          (unwind-protect
              (progn
                (store-data-test size)
                (store-string-test)
                (store-object-test))
            (format *error-output* "Restoring data.~%")
            (store-data 0 backup)))))))

  
