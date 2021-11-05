;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/test/util.lisp,v 1.8 2007/12/31 01:08:48 edi Exp $

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

#+:lispworks
(defun get-env-variable-as-directory (name)
  "Retrieves the environment variable named NAME, interprets it as the
name of a directory and returns a corresponding pathname designator.
Returns NIL if such an environment variable doesn't exist."
  (lw:when-let (string (lw:environment-variable name))
    (when (plusp (length string))
      (cond ((find (char string (1- (length string))) "\\/" :test #'char=) string)
            (t (lw:string-append string "/"))))))

(defvar *tmp-dir*
  (load-time-value
    (merge-pathnames "odd-streams-test/"
                     #+:allegro (system:temporary-directory)
                     #+:lispworks (pathname (or (get-env-variable-as-directory "TEMP")
                                                (get-env-variable-as-directory "TMP")
                                                #+:win32 "C:/"
                                                #-:win32 "/tmp/"))
                     #-(or :allegro :lispworks) #p"/tmp/"))
  "The pathname of a temporary directory used for testing.")

(defparameter *number-of-failed-tests* 0
  "Counter which counts the number of failed tests.")

(defun temp-file (filename)
  "FILENAME should be a pathname designators with all components
except for the name and the type being empty and optionally a
/relative/ directory component.  The result will be a pathname with
the same name and type located in the temporary directory *TMP-DIR*.

The directory of the resulting pathname will be created if it doesn't
exist."
  (ensure-directories-exist 
   (merge-pathnames filename *tmp-dir*)))

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
           #+:lispworks-32bit
           (hcl:mark-and-sweep 3)
           (unless ,successp
             (format *error-output* "    Test failed!!!~%"))
           (terpri *error-output*)
           (force-output *error-output*))
         ,successp))))

;; LW can't indent this correctly because it's in a MACROLET
#+:lispworks
(editor:setup-indent "with-expected-error" 1 2 4)

(defun file-size (pathspec)
  "Helper function which returns the size of the file denoted by the
pathname designator PATHSPEC in octets."
  (with-open-file (in pathspec :element-type 'octet)
    (file-length in)))

(defun odd-file-size (pathspec byte-size)
  "Helper function which returns the size of the file denoted by the
pathname designator PATHSPEC in bytes of size BYTE-SIZE."
  (with-open-odd-file (in pathspec :byte-size byte-size)
    (odd-file-length in)))

(defun file-contents (odd-stream &optional max-length)
  "Helper function which returns the complete contents of the file
underlying ODD-STREAM as a list of bytes."
  ;; this is obviously very inefficient, but convenient
  (file-position odd-stream 0)
  (let* ((buffer (make-array (odd-file-length odd-stream)
                             :element-type `(unsigned-byte ,(odd-stream-byte-size odd-stream))))
         (end (read-sequence buffer odd-stream)))
    (coerce (subseq buffer 0 (min end (or max-length end))) 'list)))
