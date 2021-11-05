;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/test/util.lisp,v 1.10 2008/04/27 20:11:56 edi Exp $

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

(defvar *test-with-dongle-p* t
  "Whether the tests should assume that a dongle is present.")

(defvar *modify-dongle-keys-p* t
  "Whether the tests may modify keys of the dongle.  This value is
irrelevant for dongles of type 2 which have only one fixed key.")

(defvar *test-authentication-code* *demo-authentication-code*
  "Authentication code used for the tests.")

(defun modify-dongle-keys-p ()
  "Returns true if the tests assume that a dongle is present /and/ if
they may modify its keys."
  (and *test-with-dongle-p*
       *modify-dongle-keys-p*))

(defun random-u32 ()
  "Returns a random unsigned long."
  (random #.(expt 2 32)))

(defun random-string (&optional (length 10))
  "Returns a random string of length LENGTH."
  (with-output-to-string (out nil :element-type 'simple-char)
    (loop repeat length do
          (write-char (code-char (random char-code-limit)) out))))

(defun sequence-equal (sequence-1 sequence-2 &optional (test 'eql))
  "Returns a true value iff the two sequences have the same length and
each two corresponding elements are equal according to TEST."
  (and (= (length sequence-1) (length sequence-2))
       (every test sequence-1 sequence-2)))

(defun get-env-variable-as-directory (name)
  "Retrieves the environment variable named NAME, interprets it as the
name of a directory and returns a corresponding pathname designator.
Returns NIL if such an environment variable doesn't exist."
  (when-let (string (environment-variable name))
    (when (plusp (length string))
      (cond ((find (char string (1- (length string))) "\\/" :test #'char=) string)
            (t (string-append string "/"))))))

(defvar *tmp-dir*
  (load-time-value
    (merge-pathnames "cl-dongle-test/"
                     (pathname (or (get-env-variable-as-directory "TEMP")
                                   (get-env-variable-as-directory "TMP")
                                   #+:win32 "C:/"
                                   #-:win32 "/tmp/"))))
  "The pathname of a temporary directory used for testing.")

(defun temp-file (filename)
  "FILENAME should be a pathname designators with all components
except for the name and the type being empty and optionally a
/relative/ directory component.  The result will be a pathname with
the same name and type located in the temporary directory *TMP-DIR*.

The directory of the resulting pathname will be created if it doesn't
exist."
  (ensure-directories-exist 
   (merge-pathnames filename *tmp-dir*)))

(defparameter *number-of-failed-tests* 0
  "Counter which counts the number of failed tests.")

(defmacro with-test ((test-description) &body body)
  "Defines a test.  Three utilities are available inside of the body
of the macro: The function FAIL and the macros CHECK and
WITH-EXPECTED-ERROR.  FAIL, the lowest level utility, marks the test
defined by WITH-TEST as failed.  CHECK checks whether its argument is
true, otherwise it calls FAIL.  If during evaluation of the specified
expression any condition is signaled, this is also considered a
failure.  WITH-EXPECTED-ERROR uses CHECK and HANDLER-CASE to execute
some forms and check whether a specified error condition was
signalled.

WITH-TEST prints reports while the tests run."
  (let ((successp (gensym)))
    `(let ((,successp t))
       (flet ((fail (format-str &rest format-args)
                (setf ,successp nil)
                (incf *number-of-failed-tests*)
                (apply #'format *error-output* format-str format-args)
                (break)))
         (macrolet ((check (expression)
                      `(handler-case
                           (unless ,expression
                             (fail "Expression ~S failed.~%" ',expression))
                         (condition (c)
                           (fail "Expression ~S failed signaling condition of type ~A: ~A~%"
                                 ',expression (type-of c) c))))
                    (with-expected-error ((condition-type) &body body)
                      `(handler-case (progn ,@body)
                         (,condition-type () t)
                         (:no-error (&rest args)
                           (declare (ignore args))                           
                           (fail "Expected condition ~S not signalled~%"
                                 ',condition-type)))))
           (format *error-output* "Test ~S~%" ,test-description)
           ,@body
           (unless ,successp
             (format *error-output* "    Test failed!!!~%"))
           (terpri *error-output*)
           (force-output *error-output*))
         ,successp))))

;; LW can't indent this correctly because it's in a MACROLET
(editor:setup-indent "with-expected-error" 1 2 4)

(defun run-tests (&key ((:authentication-code *test-authentication-code*) *test-authentication-code*)
                       ((:test-with-dongle-p *test-with-dongle-p*)
                        (yes-or-no-p "Shall the CL-DONGLE tests assume the presence of a dongle?"))
                       ((:modify-dongle-keys-p *modify-dongle-keys-p*)
                        (and *test-with-dongle-p*
                             (yes-or-no-p "Are the CL-DONGLE tests allowed to modify dongle keys?"))))
  "Runs all tests using the authentication code AUTHENTICATION-CODE."
  (authentication-test)
  (dongle-presence-test)
  (run-crypt-tests)
  (product-id-test)
  (counter-tests)
  (storage-tests)
  (run-sign-tests)
  (dongle-info-test))