;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/test/crypt.lisp,v 1.5 2008/04/27 15:08:09 edi Exp $

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

(defun tea-test (in encipher-out decipher-out &optional (key (subseq *demo-authentication-code* 0 4)))
  (with-test ((format nil "Testing TEA encryption/decryption with known input ~S." in))
    (check (sequence-equal (tea-encrypt key in) encipher-out))
    (check (sequence-equal (tea-decrypt key in) decipher-out))
    (check (sequence-equal (tea-encrypt key (coerce in 'vector)) encipher-out))
    (check (sequence-equal (tea-decrypt key (coerce in 'vector)) decipher-out))
    (let ((data (make-static-u32-array in)))
      (tea-encrypt key data :copyp nil)
      (check (sequence-equal data encipher-out)))
    (let ((data (make-static-u32-array in)))
      (tea-decrypt key data :copyp nil)
      (check (sequence-equal data decipher-out)))
    (with-dynamic-foreign-objects
        ((data :unsigned-long :initial-contents in))
      (tea-encrypt key data :length (length in))
      (dotimes (i (length encipher-out))
        (check (= (nth i encipher-out)
                  (dereference data :index i)))))
    (with-dynamic-foreign-objects
        ((data :unsigned-long :initial-contents in))
      (tea-decrypt key data :length (length in))
      (dotimes (i (length decipher-out))
        (check (= (nth i decipher-out)
                  (dereference data :index i)))))))
      
(defun run-tea-tests ()
  ;; known values from tests run against the C library
  (tea-test '(#x6a42365d #x72896eb9) '(#xf73892e #x95868d70) '(#xcae34a26 #xceed57b7))
  (tea-test '(#x6a480a56 #x7e1a2ab7) '(#x9c59983d #x8515fa89) '(#xa1939eb0 #xf320717e))
  (tea-test '(#x6a4f5e4f #x9aa66b6) '(#x441d66f3 #x80001559) '(#xec6a2eaa #x73667ebf))
  (tea-test '(#x6a585c44 #x5b0200b4) '(#xa8fb2240 #x94cfd8f6) '(#x719f8833 #xd439e7fe))
  (tea-test '(#x6a6c582f #x7db334b0) '(#xfcb6ffda #x4d6f24d2) '(#x3467c648 #x1bad39df))
  (tea-test '(#x6a6f022b #x437c12af) '(#x64667bd8 #x79327c46) '(#xe5687996 #x8c4a49d5))
  (let ((key '(#x5732d17b #xa94428ee #xf2857a8f #x69346b4a)))
    (tea-test '(#x6bc63ab2 #x62962067) '(#xcb5b8deb #x7adc6097) '(#x6466d16c #xf4eaa6a8) key)
    (tea-test '(#x6bc964ae #x285e7e66) '(#xc03c3754 #x80abca3e) '(#x3e2aaaca #x40cebc0e) key)
    (tea-test '(#x6bcd0eaa #x6e265c66) '(#xdc521caf #x8032488d) '(#x6a800a71 #xd15cf2ac) key)
    (tea-test '(#x6bd038a7 #x33ee3a65) '(#x75cc05b8 #xba06c92f) '(#x78f6cd7e #x21060673) key)))

(defun symmetric-tea-test (in key)
  (with-test ((format nil "Testing TEA encryption/decryption with random input ~S." in))
    (check (sequence-equal (tea-encrypt key (tea-decrypt key in)) in))
    (check (sequence-equal (tea-decrypt key (tea-encrypt key in)) in))))

(defun run-symmetric-tea-tests (&optional (n 20))
  (dotimes (i n)
    (let ((key (random-key))
          (in (loop repeat 2 collect (random-u32))))
      (symmetric-tea-test in key))))

(defun tea-string-tests (&optional (n 20))
  (with-test ((format nil "Testing TEA encryption/decryption with ~A random strings." n))
    (dotimes (i n)
      (let ((string (random-string 20))
            (key (random-key)))
        (check (string= (tea-decrypt-to-string key (tea-encrypt-string key string)) string))))))

(defun tea-lisp-object-tests (&optional (n 20))
  (with-test ("Testing TEA encryption/decryption with a few random Lisp objects.")
    (format *error-output* "~:(~R~) floating point numbers.~%" (* n 3))
    (dolist (prototype '(1f10 1s10 1d10))
      (dotimes (i n)
        (let* ((key (random-key))
               (factor (expt 10 (- 10 (random 20))))
               (float (* (if (plusp (random 2)) 1 -1)
                         factor
                         (random prototype))))
          (check (eql (tea-decrypt-to-lisp-object key (tea-encrypt-lisp-object key float)) float)))))
    (format *error-output* "~:(~R~) integers.~%" n)
    (dotimes (i n)
      (let ((key (random-key))
            (integer (* (if (plusp (random 2)) 1 -1)
                        (random #.(* 2 most-positive-fixnum)))))
        (check (eql (tea-decrypt-to-lisp-object key (tea-encrypt-lisp-object key integer)) integer))))
    (format *error-output* "~:(~R~) symbols.~%" n)
    (dotimes (i n)
      (let* ((key (random-key))
             (packages (list-all-packages))
             (package (nth (random (length packages)) packages))
             (symbols (loop for symbol being the symbols of package collect symbol))
             (symbol (and symbols (nth (random (length symbols)) symbols))))
        (check (eql (tea-decrypt-to-lisp-object key (tea-encrypt-lisp-object key symbol)) symbol))))))

(defun dongle-crypt-test (in encipher-out decipher-out &optional (key (subseq *demo-authentication-code* 0 4)))
  (with-test ((format nil "Testing dongle encryption/decryption with known input ~S." in))    
    (write-key 0 key)
    (check (sequence-equal (dongle-encrypt 0 in) encipher-out))
    (check (sequence-equal (dongle-decrypt 0 in) decipher-out))
    (check (sequence-equal (dongle-encrypt 0 (coerce in 'vector)) encipher-out))
    (check (sequence-equal (dongle-decrypt 0 (coerce in 'vector)) decipher-out))
    (let ((data (make-static-u32-array in)))
      (dongle-encrypt 0 data :copyp nil)
      (check (sequence-equal data encipher-out)))
    (let ((data (make-static-u32-array in)))
      (dongle-decrypt 0 data :copyp nil)
      (check (sequence-equal data decipher-out)))
    (with-dynamic-foreign-objects
        ((data :unsigned-long :initial-contents in))
      (dongle-encrypt 0 data :length (length in))
      (dotimes (i (length encipher-out))
        (check (= (nth i encipher-out)
                  (dereference data :index i)))))
    (with-dynamic-foreign-objects
        ((data :unsigned-long :initial-contents in))
      (dongle-decrypt 0 data :length (length in))
      (dotimes (i (length decipher-out))
        (check (= (nth i decipher-out)
                  (dereference data :index i)))))))
      
(defun run-dongle-crypt-tests ()
  ;; known values from tests run against the C library
  (dongle-crypt-test '(#x6a42365d #x72896eb9) '(#xf73892e #x95868d70) '(#xcae34a26 #xceed57b7))
  (dongle-crypt-test '(#x6a480a56 #x7e1a2ab7) '(#x9c59983d #x8515fa89) '(#xa1939eb0 #xf320717e))
  (dongle-crypt-test '(#x6a4f5e4f #x9aa66b6) '(#x441d66f3 #x80001559) '(#xec6a2eaa #x73667ebf))
  (dongle-crypt-test '(#x6a585c44 #x5b0200b4) '(#xa8fb2240 #x94cfd8f6) '(#x719f8833 #xd439e7fe))
  (dongle-crypt-test '(#x6a6c582f #x7db334b0) '(#xfcb6ffda #x4d6f24d2) '(#x3467c648 #x1bad39df))
  (dongle-crypt-test '(#x6a6f022b #x437c12af) '(#x64667bd8 #x79327c46) '(#xe5687996 #x8c4a49d5))
  (let ((key '(#x5732d17b #xa94428ee #xf2857a8f #x69346b4a)))
    (dongle-crypt-test '(#x6bc63ab2 #x62962067) '(#xcb5b8deb #x7adc6097) '(#x6466d16c #xf4eaa6a8) key)
    (dongle-crypt-test '(#x6bc964ae #x285e7e66) '(#xc03c3754 #x80abca3e) '(#x3e2aaaca #x40cebc0e) key)
    (dongle-crypt-test '(#x6bcd0eaa #x6e265c66) '(#xdc521caf #x8032488d) '(#x6a800a71 #xd15cf2ac) key)
    (dongle-crypt-test '(#x6bd038a7 #x33ee3a65) '(#x75cc05b8 #xba06c92f) '(#x78f6cd7e #x21060673) key)))

(defun symmetric-dongle-crypt-test (in key dongle-type)
  (with-test ((format nil "Testing dongle encryption/decryption with random input ~S." in))
    (when (> dongle-type 2)
      (write-key 0 key))
    (check (sequence-equal (dongle-encrypt 0 (dongle-decrypt 0 in)) in))
    (check (sequence-equal (dongle-decrypt 0 (dongle-encrypt 0 in)) in))))

(defun run-symmetric-dongle-crypt-tests (&key dongle-type (n 20))
  (dotimes (i n)
    (let ((key (random-key))
          (in (loop repeat 2 collect (random-u32))))
      (symmetric-dongle-crypt-test in key dongle-type))))

(defun dongle-crypt-string-tests (&key dongle-type (n 20))
  (with-test ((format nil "Testing dongle encryption/decryption with ~A random strings." n))
    (dotimes (i n)
      (let ((string (random-string 20))
            (key (random-key)))
        (when (> dongle-type 2)
          (write-key 0 key))
        (check (string= (dongle-decrypt-to-string 0 (dongle-encrypt-string 0 string)) string))))))

(defun dongle-crypt-lisp-object-tests (&key dongle-type (n 20))
  (with-test ("Testing dongle encryption/decryption with a few random Lisp objects.")
    (format *error-output* "~:(~R~) floating point numbers.~%" (* n 3))
    (dolist (prototype '(1f10 1s10 1d10))
      (dotimes (i n)
        (let* ((key (random-key))
               (factor (expt 10 (- 10 (random 20))))
               (float (* (if (plusp (random 2)) 1 -1)
                         factor
                         (random prototype))))
          (when (> dongle-type 2)
            (write-key 0 key))
          (check (eql (dongle-decrypt-to-lisp-object 0 (dongle-encrypt-lisp-object 0 float)) float)))))
    (format *error-output* "~:(~R~) integers.~%" n)
    (dotimes (i n)
      (let ((key (random-key))
            (integer (* (if (plusp (random 2)) 1 -1)
                        (random #.(* 2 most-positive-fixnum)))))
        (when (> dongle-type 2)
          (write-key 0 key))
        (check (eql (dongle-decrypt-to-lisp-object 0 (dongle-encrypt-lisp-object 0 integer)) integer))))
    (format *error-output* "~:(~R~) symbols.~%" n)
    (dotimes (i n)
      (let* ((key (random-key))
             (packages (list-all-packages))
             (package (nth (random (length packages)) packages))
             (symbols (loop for symbol being the symbols of package collect symbol))
             (symbol (and symbols (nth (random (length symbols)) symbols))))
        (when (> dongle-type 2)
          (write-key 0 key))
        (check (eql (dongle-decrypt-to-lisp-object 0 (dongle-encrypt-lisp-object 0 symbol)) symbol))))))

(defun mixed-crypt-test (in key)
  (with-test ((format nil "Testing mixed \(dongle/RAM) encryption/decryption with random input ~S." in))
    (write-key 0 key)
    (check (sequence-equal (dongle-encrypt 0 (tea-decrypt key in)) in))
    (check (sequence-equal (tea-encrypt key (dongle-decrypt 0 in)) in))
    (check (sequence-equal (dongle-decrypt 0 (tea-encrypt key in)) in))
    (check (sequence-equal (tea-decrypt key (dongle-encrypt 0 in)) in))))

(defun run-mixed-crypt-tests (&optional (n 20))
  (dotimes (i n)
    (let ((key (random-key))
          (in (loop repeat 2 collect (random-u32))))
      (mixed-crypt-test in key))))

(defun mixed-crypt-string-test (string key)
  (with-test ((format nil "Testing mixed \(dongle/RAM) encryption/decryption with a random string."))
    (write-key 0 key)
    (check (string= (dongle-decrypt-to-string 0 (tea-encrypt-string key string)) string))
    (check (string= (tea-decrypt-to-string key (dongle-encrypt-string 0 string)) string))))

(defun run-mixed-crypt-string-tests (&optional (n 20))
  (dotimes (i n)
    (let ((key (random-key))
          (string (random-string 20)))
      (mixed-crypt-string-test string key))))

(defun mixed-crypt-lisp-object-tests (&optional (n 20))
  (with-test ("Testing mixed \(dongle/RAM) encryption/decryption with a few random Lisp objects.")
    (format *error-output* "~:(~R~) floating point numbers.~%" (* n 3))
    (dolist (prototype '(1f10 1s10 1d10))
      (dotimes (i n)
        (let* ((key (random-key))
               (factor (expt 10 (- 10 (random 20))))
               (float (* (if (plusp (random 2)) 1 -1)
                         factor
                         (random prototype))))
          (write-key 0 key)
          (check (eql (dongle-decrypt-to-lisp-object 0 (tea-encrypt-lisp-object key float)) float))
          (check (eql (tea-decrypt-to-lisp-object key (dongle-encrypt-lisp-object 0 float)) float)))))
    (format *error-output* "~:(~R~) integers.~%" n)
    (dotimes (i n)
      (let ((key (random-key))
            (integer (* (if (plusp (random 2)) 1 -1)
                        (random #.(* 2 most-positive-fixnum)))))
        (write-key 0 key)
        (check (eql (dongle-decrypt-to-lisp-object 0 (tea-encrypt-lisp-object key integer)) integer))
        (check (eql (tea-decrypt-to-lisp-object key (dongle-encrypt-lisp-object 0 integer)) integer))))
    (format *error-output* "~:(~R~) symbols.~%" n)
    (dotimes (i n)
      (let* ((key (random-key))
             (packages (list-all-packages))
             (package (nth (random (length packages)) packages))
             (symbols (loop for symbol being the symbols of package collect symbol))
             (symbol (and symbols (nth (random (length symbols)) symbols))))
        (write-key 0 key)
        (check (eql (dongle-decrypt-to-lisp-object 0 (tea-encrypt-lisp-object key symbol)) symbol))
        (check (eql (tea-decrypt-to-lisp-object key (dongle-encrypt-lisp-object 0 symbol)) symbol))))))

(defun run-crypt-tests ()
  (run-tea-tests)
  (run-symmetric-tea-tests)
  (tea-string-tests)
  (tea-lisp-object-tests)
  (let ((dongle-type (dongle-type)))
    (run-symmetric-dongle-crypt-tests :dongle-type dongle-type)
    (when (modify-dongle-keys-p)
      (when (> dongle-type 2)
        (run-dongle-crypt-tests))
      (dongle-crypt-string-tests :dongle-type dongle-type)
      (dongle-crypt-lisp-object-tests :dongle-type dongle-type)
      (when (> dongle-type 2)
        (run-mixed-crypt-tests)
        (run-mixed-crypt-string-tests)
        (mixed-crypt-lisp-object-tests)))))
