;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/test/sign.lisp,v 1.1 2008/04/27 14:55:33 edi Exp $

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

(defun sign-test (interval-1 interval-2 key key-number &optional (data-length 30))
  (with-test ((format nil "Signing data with ~:[intervals ~A and ~A~;fixed interval ~A~]."
                      (eql interval-1 interval-2) interval-1 interval-2))
    (unless (and (null interval-1) (null interval-2))
      (write-key key-number key))
    (format *error-output* "A block of ~A dwords as an array, a list, and as foreign data.~%" data-length)
    (let* ((data-list (loop repeat data-length
                            collect (random-u32)))
           (data-vector (coerce data-list 'vector))
           (signature (sign data-list  
                            :interval interval-1
                            :key key
                            :key-number key-number)))
      (check (= signature
                (sign data-list
                      :interval interval-2
                      :key key
                      :key-number key-number)))
      (check (= signature
                (sign data-vector
                      :interval interval-2
                      :key key
                      :key-number key-number)))
      (with-dynamic-foreign-objects
          ((data-pointer :unsigned-long :initial-contents data-list))
        (check (= signature
                  (sign data-pointer
                        :length (* 4 data-length)
                        :interval interval-2
                        :key key
                        :key-number key-number)))))
    (let ((octet-length (+ 1 (* 4 (+ 100 (random 100))) (random 3))))
      (format *error-output* "A block of ~A octets as a file and as foreign data.~%" octet-length)
      (let* ((octets (loop repeat octet-length collect (random #.(expt 2 8))))
             (signature
              (with-dynamic-foreign-objects
                  ((octet-pointer :unsigned-byte :initial-contents octets))
                (sign octet-pointer
                      :length octet-length
                      :interval interval-1
                      :key key
                      :key-number key-number)))
             (temp-file (temp-file "data-file")))
        (unwind-protect
            (progn
              (with-open-file (out temp-file
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
                (write-sequence octets out))
              (check (= signature
                        (sign temp-file
                              :interval interval-2
                              :key key
                              :key-number key-number)))
              (check (= signature
                        (with-open-file (in temp-file
                                            :element-type '(unsigned-byte 8))
                          (let ((buffer (make-array 40
                                                    :element-type '(unsigned-byte 8)
                                                    :allocation :static)))
                            (with-dynamic-lisp-array-pointer (octet-pointer buffer :type :unsigned-byte)
                              (loop for signature = nil then (sign octet-pointer
                                                                   :length position
                                                                   :feedback-value signature
                                                                   :interval interval-2
                                                                   :key key
                                                                   :key-number key-number)
                                    for position = (read-sequence buffer in)
                                    while (plusp position)
                                    finally (return signature))))))))
          (ignore-errors
            (delete-file temp-file)))))
    (format *error-output* "20 random Lisp objects.~%")
    (dotimes (i 20)
      (let ((object (ecase (random 3)
                      (0 (let* ((prototype (nth (random 3) '(1f10 1s10 1d10)))
                                (factor (expt 10 (- 10 (random 20)))))
                           (* (if (plusp (random 2)) 1 -1)
                              factor
                              (random prototype))))
                      (1 (* (if (plusp (random 2)) 1 -1)
                            (random #.(* 2 most-positive-fixnum))))
                      (2 (let* ((packages (list-all-packages))
                                (package (nth (random (length packages)) packages))
                                (symbols (loop for symbol being the symbols of package collect symbol)))
                           (and symbols (nth (random (length symbols)) symbols)))))))
        (check (= (sign-lisp-object object :interval interval-1 :key key :key-number key-number)
                  (sign-lisp-object object :interval interval-2 :key key :key-number key-number)))))))

(defun run-sign-tests ()  
  (sign-test nil nil (random-key) nil)
  (when (and (modify-dongle-keys-p)
             (> (dongle-type) 2))
    (let ((number-of-keys (dongle-number-of-keys)))
      (sign-test nil 0 (random-key) (random number-of-keys))
      (sign-test 0 nil (random-key) (random number-of-keys))
      (sign-test 1 2 (random-key) (random number-of-keys))
      (sign-test 2 1 (random-key) (random number-of-keys)))))
             
