;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/test/tests.lisp,v 1.11 2007/12/31 01:08:48 edi Exp $

;;; Copyright (c) 2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :odd-streams-test)

(defun read-write-tests (byte-size n)
  "Tests basic reading and writing \(bytes and sequences) and the
ODD-FILE-LENGTH function."
  (let* ((bound (ash 1 byte-size))
         (bytes (loop repeat n collect (random bound)))
         (file-name (temp-file (format nil "read-write-~A" byte-size))))
    (with-test ((format nil "Writing and reading ~A bytes of size ~A, also checking file sizes."
                        n byte-size))
      (with-open-odd-file (out file-name
                               :byte-size byte-size
                               :direction :output
                               :if-exists :supersede)
        (cond ((zerop (random 2))
               (dolist (byte bytes)
                 (write-byte byte out)))
              (t (write-sequence bytes out))))
      (let* ((expected-file-size-in-bits (* n byte-size))
             (expected-file-size-in-octets (ceiling expected-file-size-in-bits 8)))
        (check (= (file-size file-name) expected-file-size-in-octets))
        (check (= (odd-file-size file-name byte-size)
                  (floor (* expected-file-size-in-octets 8) byte-size))))
      (check (equal (with-open-odd-file (in file-name :byte-size byte-size)
                      (let* ((buffer (make-array n :element-type `(unsigned-byte ,byte-size)))
                             (end (read-sequence buffer in)))
                        (coerce (subseq buffer 0 end) 'list)))
                    bytes))
      (check (equal (with-open-odd-file (in file-name :byte-size byte-size)
                      (loop repeat n
                            for byte = (read-byte in nil)
                            while byte
                            collect byte))
                    bytes)))))

(defun position-tests (byte-size n m)
  "Mainly test the FILE-POSITION functionality."
  (let* ((bound (ash 1 byte-size))
         (bytes (loop repeat n collect (random bound)))
         (file-name (temp-file (format nil "position-~A" byte-size))))
    (with-test ((format nil "Writing ~A bytes of size ~A, then reading ~
single bytes at different file positions."
                        n byte-size))
      (with-open-odd-file (out file-name
                               :byte-size byte-size
                               :direction :output
                               :if-exists :supersede)
        (write-sequence bytes out))
      (with-open-odd-file (in file-name :byte-size byte-size)
        (dotimes (i m)
          (let ((position (random n)))
            (file-position in position)
            (check (= (read-byte in) (nth position bytes)))))
        ;; trying the first byte
        (file-position in 0)
        (check (= (read-byte in) (first bytes)))
        (when (> n 1)
          ;; and maybe the second one
          (check (= (read-byte in) (second bytes))))
        ;; trying the last byte
        (file-position in (1- n))
        (check (= (read-byte in) (car (last bytes))))))))

(defun conversion-tests (byte-size n factor)
  "Tests \"compatibility\" between byte sizes which are a multiple of
each other."
  (let* ((bound (ash 1 byte-size))
         (bytes (loop repeat (* n factor) collect (random bound)))
         (file-name (temp-file (format nil "conversion-~A-~A" byte-size factor))))
    (with-test ((format nil "Writing ~A bytes of size ~A, then reading ~
them as bytes of size ~A."
                        (* n factor) byte-size (* factor byte-size)))
      (with-open-odd-file (out file-name
                               :byte-size byte-size
                               :direction :output
                               :if-exists :supersede)
        (write-sequence bytes out))
      (with-open-odd-file (in file-name :byte-size (* factor byte-size))
        (dotimes (i n)
          (let ((expected-result
                 (loop repeat factor
                       for index from (* i factor)
                       for power = 1 then (* power bound)
                       sum (* (nth index bytes) power))))
            (check (= expected-result (read-byte in)))))))))

#+:little-endian
(defun consistency-tests (byte-size n)
  "Tests \"compatibility\" with standard CL file functions.  Only
useful on little endian platforms."
  (let* ((bound (ash 1 byte-size))
         (bytes (loop repeat n collect (random bound)))
         (file-name (temp-file (format nil "consistency-~A" byte-size))))
    (with-test ((format nil "Writing ~A bytes of size ~A, then reading ~
them back in using ANSI Common Lisp functionality."
                        n byte-size))
      (with-open-odd-file (out file-name
                               :byte-size byte-size
                               :direction :output
                               :if-exists :supersede)
        (write-sequence bytes out))
      (let ((element-type `(unsigned-byte ,byte-size)))
        (with-open-file (in file-name :element-type element-type)
          (let* ((buffer (make-array n :element-type element-type))
                 (end (read-sequence buffer in)))
            (check (equal bytes (coerce (subseq buffer 0 end) 'list)))))))))

(defun io-tests (byte-size n m)
  "Tests working with IO files - reading, writing, modifying..."
  (let* ((bound (ash 1 byte-size))
         (bytes (loop repeat (* 2 n) collect (random bound)))
         (file-name (temp-file (format nil "io-~A" byte-size))))
    (with-test ((format nil "Writing and reading at the same time, ~
swapping bytes - with ~A bytes of size ~A."
                        (* 2 n) byte-size))
      (with-open-odd-file (io file-name
                              :byte-size byte-size
                              :direction :io
                              :if-exists :supersede)
        (write-sequence bytes io)
        (file-position io 0)
        (dotimes (i n)
          (rotatef (nth (* 2 i) bytes) (nth (1+ (* 2 i)) bytes))
          (let ((two-bytes (list (read-byte io) (read-byte io))))
            (file-position io (- (file-position io) 2))
            (write-sequence (reverse two-bytes) io))))
      (with-open-odd-file (in file-name :byte-size byte-size)
        (check (equal bytes (file-contents in (length bytes)))))
      (when (> n 1)
        (with-open-odd-file (io file-name
                                :byte-size byte-size
                                :direction :io
                                :if-exists :overwrite)
          (dotimes (i m)
            (let* ((pos-1 (random n))
                   (pos-2 (loop for try = (random n) while (= pos-1 try) finally (return try))))
              (file-position io pos-1)
              (let ((byte-1 (read-byte io)))
                (file-position io pos-2)
                (let ((byte-2 (read-byte io)))
                  (file-position io pos-2)
                  (write-byte byte-1 io)
                  (file-position io pos-1)
                  (write-byte byte-2 io)))
              (rotatef (nth pos-1 bytes) (nth pos-2 bytes))))
          (check (equal bytes (file-contents io (length bytes)))))
        (with-open-odd-file (in file-name
                                :byte-size byte-size
                                :direction :io
                                :if-exists :overwrite)
          (check (equal bytes (file-contents in (length bytes)))))))))

(defun clean-up ()
  "Deletes all the files that were created during the tests."
  (dolist (file (directory (make-pathname :name :wild
                                          :type :wild
                                          :version :wild
                                          :defaults *tmp-dir*)))
    (ignore-errors (delete-file file)))
  #+:lispworks
  (ignore-errors 
    (lw:delete-directory *tmp-dir*)))

(defun run-tests ()
  "Runs all tests defined above with several different parameters and
writes reports to *ERROR-OUTPUT*.  Might take a pretty long time."
  (setq *number-of-failed-tests* 0)
  (dotimes (i 200)
    (read-write-tests (1+ (random odd-streams::+max-byte-size+))
                      (random 10000)))
  (dotimes (i 200)
    (position-tests (1+ (random odd-streams::+max-byte-size+))
                    (1+ (random 10000))
                    50))
  (dolist (byte-size `(1 7 8 9 15 16 17 ,odd-streams::+max-byte-size+))
    (dolist (n '(0 1 10 100 1000 10000))
      (read-write-tests byte-size n)
      (when (plusp n)
        (position-tests byte-size n 100))))
  (dotimes (i 10)
    (let ((factor (+ 2 (random 5))))
      (conversion-tests (1+ (random (floor odd-streams::+max-byte-size+ factor))) 100 factor)))
  #+:little-endian
  (dolist (byte-size '(8 16 32))
    (consistency-tests byte-size 5000))
  (dotimes (i 50)
    (io-tests (1+ (random odd-streams::+max-byte-size+))
              (random 10000)
              100))
  ;; regression test for two cases which failed during development
  (io-tests 113 10000 50)
  (io-tests 1 8486 0)
  (cond ((plusp *number-of-failed-tests*)
         (format *error-output* "~&~%~A tests failed....~%"
                 *number-of-failed-tests*))
        (t (format *error-output* "~&~%All tests passed.~%")))
  (clean-up)
  (values))
