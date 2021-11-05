;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/serialize.lisp,v 1.7 2008/04/18 07:53:29 edi Exp $

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

(defun pack-16-bit-bytes (byte-1 byte-2)
  "Returns an unsigned long containing the 16-bit words BYTE-1 and
BYTE-2.  BYTE-2 can be NIL in which case it is treated as if it were
zero."
  (let ((unsigned-long 0))
    (setf (ldb (byte 16 0) unsigned-long) byte-1
          (ldb (byte 16 16) unsigned-long) (or byte-2 0))
    unsigned-long))

(defun unpack-16-bit-bytes (unsigned-long)
  "The inverse of PACK-16-BIT-BYTES.  Returns a list of two integers
corresponding to the BYTE-1 and BYTE-2 arguments there."
  (list (ldb (byte 16 0)unsigned-long)
        (ldb (byte 16 16) unsigned-long)))

(defun string-to-unsigned-longs (string &optional padp)
  "Encodes the Lisp string STRING as a list of '\(UNSIGNED-BYTE 32)
integers denoting \"packed\" pairs of '\(UNSIGNED-BYTE 16) integers.
The first integer is the length of the string.  If PADP is true, the
result is padded with a zero if necessary to ensure that the resulting
list has an even number of elements."
  (loop for oddp = t then (not oddp)
        for (byte-1 byte-2 . rest)
        ;; on LispWorks, CHAR-CODE-LIMIT is 2^16
        on (list* (length string) (map 'list 'char-code string))
        by 'cddr
        collect (pack-16-bit-bytes byte-1 byte-2)
        when (and padp oddp (null rest))
        collect 0))

(defgeneric unsigned-longs-to-string (unsigned-longs)
  (:documentation "The inverse of STRING-TO-UNSIGNED-LONGS.  Converts
a sequence of unsigned longs back to a string.

Note that this function obviously doesn't work with all sequences but
only with those which were created using STRING-TO-UNSIGNED-LONGS."))

(defmethod unsigned-longs-to-string ((unsigned-longs list))
  (let ((bytes (loop for unsigned-long in unsigned-longs
                     for (byte-1 byte-2) = (unpack-16-bit-bytes unsigned-long)
                     collect byte-1
                     collect byte-2)))
    (map 'simple-text-string 'code-char (subseq bytes 1 (1+ (first bytes))))))

(defmethod unsigned-longs-to-string ((unsigned-longs vector))
  (let ((bytes (loop for unsigned-long across unsigned-longs
                     for (byte-1 byte-2) = (unpack-16-bit-bytes unsigned-long)
                     collect byte-1
                     collect byte-2)))
    (map 'simple-text-string 'code-char (subseq bytes 1 (1+ (first bytes))))))

(defun serialize (object)
  "Naïve function to serialize the Lisp object OBJECT to a list of
unsigned longs via WRITE-TO-STRING."
  (string-to-unsigned-longs (with-standard-io-syntax
                              (write-to-string object))
                            t))

(defun unserialize (unsigned-longs)
  "The inverse of SERIALIZE.  Converts a sequence of unsigned longs
back to a Lisp object.

Note that this function obviously doesn't work with all sequences but
only with those which were created using SERIALIZE."
  (with-standard-io-syntax
    (values
     (read-from-string
      (unsigned-longs-to-string unsigned-longs)))))