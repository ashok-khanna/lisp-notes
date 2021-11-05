;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/util.lisp,v 1.21 2008/04/27 20:11:53 edi Exp $

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

(deftype fli-pointer ()
  "A type corresponding to LispWorks' FLI pointers."
  `(satisfies pointerp))

(defun major-minor (unsigned-long)
  "Accepts an unsigned long integer which encodes the major and minor
version of something in its high and low word respectively and returns
these two values as a list."
  (list (ldb (byte 16 16) unsigned-long)
        (ldb (byte 16 0) unsigned-long)))

(defgeneric make-static-u32-array (initial-elements &optional copyp)
  (:documentation "Returns an array of element type \(UNSIGNED-BYTE
32) allocated in the static area and filled with the initial elements
INITIAL-ELEMENTS \(a list or an array).  The length of the array is
determined by INITIAL-ELEMENTS.  If INITIAL-ELEMENTS is already a
static array and COPYP is NIL, then this array will simply be
returned."))

(defmethod make-static-u32-array ((initial-elements list) &optional copyp)
  (declare (ignore copyp))
  (make-array (length initial-elements)
              :element-type '(unsigned-byte 32)
              :allocation :static
              :initial-contents initial-elements))

(defmethod make-static-u32-array ((initial-elements vector) &optional copyp)
  (when (and (not copyp)
             (equal (array-element-type initial-elements) '(unsigned-byte 32))
             (staticp initial-elements))
    (return-from make-static-u32-array initial-elements))
  (let ((result (make-array (length initial-elements)
                            :element-type '(unsigned-byte 32)
                            :allocation :static)))
    (map-into result 'identity initial-elements)))

(defun random-key ()
  "Returns a static array of 4 random \(UNSIGNED-BYTE 32) integers,
i.e. a value which can be used as a random key within CL-DONGLE."
  (make-array 4
              :element-type '(unsigned-byte 32)
              :allocation :static
              :initial-contents (loop repeat 4
                                      collect (random #.(expt 2 32)))))

(defmacro int32-logxor* (x y &rest more-args)
  "Utility macro to replace INT32-LOGXOR with an operation with
arbitrary arity."
  (if (null more-args)
    `(int32-logxor ,x ,y)
    `(int32-logxor ,x (int32-logxor* ,y ,@more-args))))

(defmacro int32-incf (place &optional (delta '+int32-1+))
  "Like INCF, but for INT32 objects."
  `(setf ,place (int32+ ,place ,delta)))

(defmacro int32-decf (place &optional (delta '+int32-1+))
  "Like DECF, but for INT32 objects."
  `(setf ,place (int32- ,place ,delta)))

(defmacro int32-lsr (x c)
  "Simulates a `logical shift right' by performing an arithmetic shift
and then masking out the sign bit."
  `(int32-logand (int32>> ,x ,c) #x7FFFFFF))

(declaim (inline sgl-tea-encipher))
(defun sgl-tea-encipher (in-data out-data key)
  "Encrypts IN-DATA to OUT-DATA according to the Tiny Encryption
Algorithm using the key KEY.

IN-DATA and OUT-DATA must be FLI pointers to two-element arrays of
unsigned longs, KEY must be an FLI pointer to a four-element array of
unsigned longs.  IN-DATA may be equal to OUT-DATA.

See <http://en.wikipedia.org/wiki/Tiny_Encryption_Algorithm> and the
function SglTeaEncipher in SglWin32.h."
  ;; this function is obviously highly optimized and needs only about
  ;; 60% more time than Visual Studio "release" C code
  (declare (optimize (speed 3) (safety 0) (float 0)))
  (let ((y (foreign-typed-aref 'int32 in-data 0))
        (z (foreign-typed-aref 'int32 in-data 4))
        (sum +int32-0+)
        (a (foreign-typed-aref 'int32 key 0))
        (b (foreign-typed-aref 'int32 key 4))
        (c (foreign-typed-aref 'int32 key 8))
        (d (foreign-typed-aref 'int32 key 12)))
    (declare (type int32 y z sum))
    (loop repeat 32 do
          (int32-incf sum #.+tea-delta+)
          (int32-incf y (int32-logxor* (int32+ (int32<< z 4) a)
                                       (int32+ z sum)
                                       (int32+ (int32-lsr z 5) b)))
          (int32-incf z (int32-logxor* (int32+ (int32<< y 4) c)
                                       (int32+ y sum)
                                       (int32+ (int32-lsr y 5) d))))
    (setf (foreign-typed-aref 'int32 out-data 0) y
          (foreign-typed-aref 'int32 out-data 4) z)
    nil))

(declaim (inline sgl-tea-decipher))
(defun sgl-tea-decipher (in-data out-data key)
  "The inverse of SGL-TEA-ENCIPHER.  See docstring there."
  (declare (optimize (speed 3) (safety 0) (float 0)))
  (let ((y (foreign-typed-aref 'int32 in-data 0))
        (z (foreign-typed-aref 'int32 in-data 4))
        (sum #.+tea-sum+)
        (a (foreign-typed-aref 'int32 key 0))
        (b (foreign-typed-aref 'int32 key 4))
        (c (foreign-typed-aref 'int32 key 8))
        (d (foreign-typed-aref 'int32 key 12)))
    (declare (type int32 y z sum))
    (loop repeat 32 do
          (int32-decf z (int32-logxor* (int32+ (int32<< y 4) c)
                                       (int32+ y sum)
                                       (int32+ (int32-lsr y 5) d)))
          (int32-decf y (int32-logxor* (int32+ (int32<< z 4) a)
                                       (int32+ z sum)
                                       (int32+ (int32-lsr z 5) b)))
          (int32-decf sum #.+tea-delta+))
    (setf (foreign-typed-aref 'int32 out-data 0) y
          (foreign-typed-aref 'int32 out-data 4) z)
    nil))

(defun sgl-sign-data-comb (product-id key-pointer key-number interval length data-pointer feedback-register)
  "Signs LENGTH octets starting at position DATA-POINTER \(an FLI
pointer) using the Tiny Encryption Algorithm.  LENGTH must be a fixnum
divisible by 8, i.e. this function actually operates on 64-bit blocks!
Signing is done by continously updating the value in FEEDBACK-REGISTER
\(which must be an FLI pointer pointing to two adjacent unsigned
longs), so the outcome of this function depends on the initial value
of FEEDBACK-REGISTER.  Also, the return value of this function is
irrelevant, the actual \"return value\" is what FEEDBACK-REGISTER
points to.

Signing is done in RAM or in the dongle or both depending on the value
of INTERVAL - see the docstring of SIGN for details.  KEY-POINTER must
be an FLI pointer to a 128-bit key as in SGL-TEA-ENCIPHER.  This
argument is only used if encryption happens in RAM.  PRODUCT-ID and
KEY-NUMBER must be fixnums denoting a specific dongle and the key in
this dongle to be used.  These arguments are only used if encryption
happens in the dongle.

See the function SglSignDataComb in SglWin32.h \(which does something
similar but not exactly the same)."
  (declare (optimize (speed 3) (safety 0) (float 0) (fixnum-safety 0)))
  (let ((index 0)
        (mask (and interval (1- (ash 1 (+ interval 3))))))
    (loop
     (unless (< index length)
       (return-from sgl-sign-data-comb))
     (let ((use-dongle-p (and mask (zerop (logand index mask)))))
       (setf (foreign-typed-aref 'int32 feedback-register 0)
             (int32-logior (foreign-typed-aref 'int32 data-pointer index)
                           (foreign-typed-aref 'int32 feedback-register 0)))
       (incf index 4)
       (setf (foreign-typed-aref 'int32 feedback-register 4)
             (int32-logior (foreign-typed-aref 'int32 data-pointer index)
                           (foreign-typed-aref 'int32 feedback-register 4)))
       (incf index 4)
       (cond (use-dongle-p
              (sgl-crypt-lock key-number +sgl-crypt-mode-encrypt+ 1 feedback-register product-id))
             (t (sgl-tea-encipher feedback-register feedback-register key-pointer)))))))

(defun tea-decrypt-raw (key data-pointer length)
  "Optimized helper function to apply SGL-TEA-DECIPHER successively to
LENGTH \(a fixnum) unsigned longs starting at DATA-POINTER \(an FLI
pointer).  KEY is as in SGL-TEA-DECIPHER."
  (declare (optimize (speed 3) (safety 0) (float 0) (fixnum-safety 0)))
  (dotimes (i (ash length -1))
    (sgl-tea-decipher data-pointer data-pointer key)
    (incf-pointer data-pointer 2)))

(defun tea-encrypt-raw (key data-pointer length)
  "Optimized helper function to apply SGL-TEA-ENCIPHER successively to
LENGTH \(a fixnum) unsigned longs starting at DATA-POINTER \(an FLI
pointer).  KEY is as in SGL-TEA-ENCIPHER."
  (declare (optimize (speed 3) (safety 0) (float 0) (fixnum-safety 0)))
  (dotimes (i (ash length -1))
    (sgl-tea-encipher data-pointer data-pointer key)
    (incf-pointer data-pointer 2)))

(declaim (notinline sgl-tea-encipher))
(declaim (notinline sgl-tea-decipher))

(defun sgl-authent (authent-code-array)
  "Helper function to authenticate the Lisp code against the SG-Lock
DLL and vice versa.  See the function SglAuthent in SglWin32.h.

AUTHENT-CODE-ARRAY must be a static array containing twelve elements
of type \(UNSIGNED-BYTE 32)."
  (let ((randoms (loop repeat 2 collect (random #.(expt 2 32)))))
    (with-dynamic-lisp-array-pointer (authent-code-pointer authent-code-array :type :unsigned-long)
      (with-dynamic-foreign-objects
          ((rand-num :unsigned-long :initial-contents randoms)
           (app-rand-num :unsigned-long :initial-contents randoms)
           (lib-rand-num :unsigned-long :nelems 2))
        (sgl-authent-a authent-code-pointer app-rand-num lib-rand-num)
        (incf-pointer authent-code-pointer 8)
        (sgl-tea-encipher rand-num rand-num authent-code-pointer)
        (unless (and (= (dereference rand-num) (dereference app-rand-num))
                     (= (dereference rand-num :index 1) (dereference app-rand-num :index 1)))            
          (error 'authentication-failed))
        (sgl-tea-encipher lib-rand-num lib-rand-num authent-code-pointer)
        (sgl-authent-b lib-rand-num)))))
