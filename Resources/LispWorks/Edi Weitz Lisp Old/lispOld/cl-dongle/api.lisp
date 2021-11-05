;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-DONGLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-dongle/api.lisp,v 1.18 2008/04/27 22:08:18 edi Exp $

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

(defun authenticate (code)
  "Authenticates the Lisp application against the SG-Lock DLL and vice
versa.  CODE must be a sequence of twelve \(UNSIGNED-BYTE 32) integers
representing the authentication code you received from SG-Lock when
you bought your dongles \(or *DEMO-AUTHENTICATION-CODE* for demo
dongles).

This function must be called before any communication with the
corresponding dongle is attempted.  It returns no values in the case
of success and signals an AUTHENTICATION-FAILED error otherwise."
  (unless (= (length code) 12)
    (simple-dongle-error "Authentication code must have length 12."))
  (sgl-authent (make-static-u32-array code)))

(defun dongle-present-p (&optional (product-id *product-id*))
  "Returns T if the dongle with the product ID PRODUCT-ID is present,
NIL otherwise.

See also ASSERT-DONGLE."
  (handler-case
      (assert-dongle product-id)
    (dongle-not-found () nil)
    (:no-error () t)))

(defun (setf product-id) (new-product-id &optional (product-id *product-id*))
  "Sets the product ID of the dongle with product ID PRODUCT-ID to
NEW-PRODUCT-ID.  Note that only the least significant 16 bit of
NEW-PRODUCT-ID will actually be used.

Note that this operation does /not/ change the value of *PRODUCT-ID*."
  (sgl-write-product-id new-product-id product-id)
  new-product-id)

(defun counter-value (counter-number &optional (product-id *product-id*))
  "Returns the value of the counter COUNTER-NUMBER in the dongle with
the product ID PRODUCT-ID."
  (sgl-read-counter counter-number product-id))

(defun (setf counter-value) (new-counter-value counter-number &optional (product-id *product-id*))
  "Sets the value of the counter COUNTER-NUMBER in the dongle with the
product ID PRODUCT-ID to NEW-COUNTER-VALUE."
  (sgl-write-counter counter-number new-counter-value product-id)
  new-counter-value)

(defun write-key (key-number key &optional (product-id *product-id*))
  "Overwrites the key numbered KEY-NUMBER in the dongle with product
ID PRODUCT-ID and sets it to KEY.  KEY must be a sequence of four
\(UNSIGNED-BYTE 32) integers.  Returns KEY.

Note that for security reasons there's no inverse operation to read
the keys of a dongle."
  (unless (= (length key) 4)
    (simple-dongle-error "Key must have length 4."))
  (with-dynamic-lisp-array-pointer (key-pointer (make-static-u32-array key) :type :unsigned-long)
    (sgl-write-key key-number key-pointer product-id))
  key)

(defun dongle-type (&optional (product-id *product-id*))
  "Returns the \"type\" of the dongle with the product ID PRODUCT-ID.
The type is one of 2, 3, or 4 corresponding to the number in SG-Lock
product names like \"U2\" or \"L4\"."
  (ecase (read-config product-id 'type)
    (#.+sgl-config-lock-series-2+ 2)
    (#.+sgl-config-lock-series-3+ 3)
    (#.+sgl-config-lock-series-4+ 4)))

(defun dongle-interface (&optional (product-id *product-id*))
  "Returns the \"interface\" of the dongle with the product ID
PRODUCT-ID which is one of the keywords :USB or :LPT."
  (ecase (read-config product-id 'interface)
    (#.+sgl-config-interface-usb+ :usb)
    (#.+sgl-config-interface-lpt+ :lpt)))

(defun dongle-software-version (&optional (product-id *product-id*))
  "Returns the software version of the dongle with the product ID
PRODUCT-ID as a list of two integers - the major and minor version."
  (major-minor (read-config product-id 'software-version)))

(defun dongle-hardware-version (&optional (product-id *product-id*))
  "Returns the hardware version of the dongle with the product ID
PRODUCT-ID as a list of two integers - the major and minor version."
  (major-minor (read-config product-id 'hardware-version)))

(defun dongle-memory-size (&optional (product-id *product-id*))
  "Returns the memory size of the dongle with the product ID
PRODUCT-ID.  The size is measured in dwords, i.e. in 32-bit blocks.
So, if the return value is, say, 256, the memory size is 1024 octets."
  (read-config product-id 'memory-size))

(defun dongle-number-of-counters (&optional (product-id *product-id*))
  "Returns the number of available programmable counters of the dongle
with the product ID PRODUCT-ID."
  (read-config product-id 'number-of-counters))

(defun dongle-number-of-keys (&optional (product-id *product-id*))
  "Returns the number of available programmable keys of the dongle
with the product ID PRODUCT-ID."
  (read-config product-id 'number-of-keys))

(defun store-data (address data &key (product-id *product-id*) (start 0) end)
  "Stores the contents of the \(UNSIGNED-BYTE 32) sequence DATA from
START to END in the dongle with the product ID PRODUCT-ID starting at
memory address ADDRESS.  ADDRESS uses a 32-bit addressing scheme
corresponding to the values returned by DONGLE-MEMORY-SIZE.

Returns the address following the stored data.  \(Note that depending
on the memory size of the dongle this might be an illegal address.)"
  (let* ((static-data-array
          (make-static-u32-array
           ;; because we don't want to convert static arrays...
           (cond ((or (plusp start) end)
                  (subseq data start end))
                 (t data))))
         (length (length static-data-array)))
    (with-dynamic-lisp-array-pointer (data-pointer static-data-array :type :unsigned-long)
      (sgl-write-data address length data-pointer product-id))
    (+ address length)))

(defun retrieve-data (address count &optional (product-id *product-id*))
  "Returns COUNT values stored at the memory area starting at ADDRESS
from the dongle with the product ID PRODUCT-ID as a static array of
element-type \(UNSIGNED-BYTE 32).  ADDRESS uses a 32-bit addressing
scheme corresponding to the values returned by DONGLE-MEMORY-SIZE."
  (let ((data-array (make-array count
                                :element-type '(unsigned-byte 32)
                                :allocation :static)))
    (when (plusp count)
      (with-dynamic-lisp-array-pointer (data-pointer data-array :type :unsigned-long)
        (sgl-read-data address count data-pointer product-id)))
    data-array))

(defun store-unsigned-long (address value &optional (product-id *product-id*))
  "Stores the \(UNSIGNED-BYTE 32) integer VALUE in the dongle with the
product ID PRODUCT-ID at memory address ADDRESS.  ADDRESS uses a
32-bit addressing scheme corresponding to the values returned by
DONGLE-MEMORY-SIZE.

Returns the address following the stored data.  \(Note that depending
on the memory size of the dongle this might be an illegal address.)"
  (store-data address (list value) :product-id product-id))

(defun retrieve-unsigned-long (address &optional (product-id *product-id*))
  "Returns the \(UNSIGNED-BYTE 32) integer from the dongle with the
product ID PRODUCT-ID which is stored at memory address ADDRESS.
ADDRESS uses a 32-bit addressing scheme corresponding to the values
returned by DONGLE-MEMORY-SIZE."
  (aref (retrieve-data address 1 product-id) 0))

(defun store-string (address string &key (product-id *product-id*) (start 0) end)
  "Stores the Lisp string STRING from START to END in the dongle with
the product ID PRODUCT-ID starting at memory address ADDRESS.  ADDRESS
uses a 32-bit addressing scheme corresponding to the values returned
by DONGLE-MEMORY-SIZE.  The string is stored in such a way that a
string which is STRING= to it can be retrieved using RETRIEVE-STRING.
A string of length N will use up \(CEILING (1+ N) 2) dwords in the
dongle.

Returns the address following the stored data.  \(Note that depending
on the memory size of the dongle this might be an illegal address.)"
  (store-data address (string-to-unsigned-longs (subseq string start end))
              :product-id product-id))

(defun retrieve-string (address &optional (product-id *product-id*))
  "Returns the Lisp string from the dongle with the product ID
PRODUCT-ID which is stored at memory address ADDRESS.  Obviously, this
will only work if the string was previously stored using STORE-STRING.
ADDRESS uses a 32-bit addressing scheme corresponding to the values
returned by DONGLE-MEMORY-SIZE.

The returned string will be STRING= but not identical to the string
that was originally stored."
  (let ((string-length (first (unpack-16-bit-bytes (retrieve-unsigned-long address product-id)))))
    (unsigned-longs-to-string (coerce (retrieve-data address (1+ (floor string-length 2)) product-id)
                                      'list))))

(defun store-lisp-object (address object &optional (product-id *product-id*))
  "Stores the Lisp object OBJECT in the dongle with the product ID
PRODUCT-ID starting at memory address ADDRESS.  ADDRESS uses a 32-bit
addressing scheme corresponding to the values returned by
DONGLE-MEMORY-SIZE.  The object is stored in such a way that it can be
retrieved using RETRIEVE-LISP-OBJECT.

Objects are stored using a naïve serialization algorithm based on
WRITE-TO-STRING which is neither fast nor space-efficient.
Furthermore, only objects which can be printed readably can be stored
this way.

Returns the address following the stored data.  \(Note that depending
on the memory size of the dongle this might be an illegal address.)"
  (store-string address
                (with-standard-io-syntax
                  (write-to-string object))
                :product-id product-id))

(defun retrieve-lisp-object (address &optional (product-id *product-id*))
  "Returns the Lisp object from the dongle with the product ID
PRODUCT-ID which is stored at memory address ADDRESS.  Obviously, this
will only work if the object was previously stored using
STORE-LISP-OBJECT.  ADDRESS uses a 32-bit addressing scheme
corresponding to the values returned by DONGLE-MEMORY-SIZE.

Numbers, symbols, and characters will be EQL to the objects originally
stored.  Other objects will only be EQUAL or EQUALP.  See the
docstring of STORE-LISP-OBJECT and the code in serialize.lisp."
  (with-standard-io-syntax
    (values
     (read-from-string
      (retrieve-string address product-id)))))

(defun dongle-decrypt/encrypt (mode key-number data length product-id copyp)
  "Internal function which encrypts or decrypts DATA using the dongle
with the product ID PRODUCT-ID and the key with the number KEY-NUMBER.
MODE determines whether the data is encrypted or decrypted \(see
function SGL-CRYPT-LOCK).  DATA must either be a sequence of
\(UNSIGNED-BYTE 32) integers or an FLI pointer.  In the latter case,
LENGTH is the number of dwords to be processed.  In both cases, the
number of dwords to be processed must be even.

If DATA is a pointer, the encryption/decryption is performed in place
and the pointer is returned, otherwise a static array containing the
encrypted/decrypted elements from DATA is the return value.  If DATA
is a static array of element type \(UNSIGNED-BYTE 32) and COPYP is
NIL, the contents of DATA will also be modified in place and DATA will
be returned."
  (when (and (not (pointerp data)) length)
    (simple-dongle-error "Providing length argument doesn't make sense."))
  (when (null length)
    (when (pointerp data)
      (simple-dongle-error "No length argument provided."))
    (setq length (length data)))
  (unless (evenp length)
    (simple-dongle-error "Length must be even."))
  (when (zerop length)
    (return-from dongle-decrypt/encrypt data))
  (cond ((pointerp data)
         (sgl-crypt-lock key-number mode (/ length 2) data product-id)
         data)
        (t (let ((data-array (make-static-u32-array data copyp)))
             (with-dynamic-lisp-array-pointer (data-pointer data-array :type :unsigned-long)
               (sgl-crypt-lock key-number mode (/ length 2) data-pointer product-id))
             data-array))))

(defun tea-decrypt/encrypt (function key data length copyp)
  "This function is the in-RAM equivalent to DONGLE-ENCRYPT/DECRYPT,
so see the docstring there.  The only difference is that KEY must be a
four-element sequence of \(UNSIGNED-BYTE 32) integers representing the
key to be used and FUNCTION is a function designator for either
#'TEA-ENCRYPT-RAW or #'TEA-DECRYPT-RAW."
  (unless (= (length key) 4)
    (simple-dongle-error "Key must have length 4."))
  (when (and (not (pointerp data)) length)
    (simple-dongle-error "Providing length argument doesn't make sense."))
  (when (null length)
    (when (pointerp data)
      (simple-dongle-error "No length argument provided."))
    (setq length (length data)))
  (unless (evenp length)
    (simple-dongle-error "Length must be even."))
  (when (zerop length)
    (return-from tea-decrypt/encrypt data))
  (with-dynamic-lisp-array-pointer (key-pointer (make-static-u32-array key) :type :unsigned-long)
    (cond ((pointerp data)
           (funcall function key-pointer (copy-pointer data :type :unsigned-long) length)
           data)
          (t (let ((data-array (make-static-u32-array data copyp)))
               (with-dynamic-lisp-array-pointer (data-pointer data-array :type :unsigned-long)
                 (funcall function key-pointer data-pointer length))
               data-array)))))

(defun dongle-encrypt (key-number data &key (product-id *product-id*) length (copyp t))
  "Encrypts DATA using the dongle with the product ID PRODUCT-ID and
the key with the number KEY-NUMBER.  DATA must either be a sequence of
\(UNSIGNED-BYTE 32) integers or an FLI pointer.  In the latter case,
LENGTH is the number of dwords to be processed.  In both cases, the
number of dwords to be processed must be even.

If DATA is a pointer, the encryption is performed in place and the
pointer is returned, otherwise a static array containing the encrypted
elements from DATA is the return value.  If DATA is a static array of
element type \(UNSIGNED-BYTE 32) and COPYP is NIL, the contents of
DATA will also be modified in place and DATA will be returned.

This is the inverse of DONGLE-DECRYPT.  See also TEA-ENCRYPT."
  (dongle-decrypt/encrypt +sgl-crypt-mode-encrypt+ key-number data length product-id copyp))

(defun dongle-decrypt (key-number data &key (product-id *product-id*) length (copyp t))
  "Decrypts DATA using the dongle with the product ID PRODUCT-ID and
the key with the number KEY-NUMBER.  DATA must either be a sequence of
\(UNSIGNED-BYTE 32) integers or an FLI pointer.  In the latter case,
LENGTH is the number of dwords to be processed.  In both cases, the
number of dwords to be processed must be even.

If DATA is a pointer, the decryption is performed in place and the
pointer is returned, otherwise a static array containing the decrypted
elements from DATA is the return value.  If DATA is a static array of
element type \(UNSIGNED-BYTE 32) and COPYP is NIL, the contents of
DATA will also be modified in place and DATA will be returned.

This is the inverse of DONGLE-ENCRYPT.  See also TEA-DECRYPT."
  (dongle-decrypt/encrypt +sgl-crypt-mode-decrypt+ key-number data length product-id copyp))

(defun dongle-encrypt-string (key-number string &key (product-id *product-id*))
  "Encrypts the Lisp string STRING using the dongle with the product
ID PRODUCT-ID and the key with the number KEY-NUMBER.  The return
value is a \(static) array of integers of type \(UNSIGNED-BYTE 32)
which can be \(given the right key) decrypted back to a string using
DONGLE-DECRYPT-TO-STRING or TEA-DECRYPT-TO-STRING."
  (dongle-decrypt/encrypt +sgl-crypt-mode-encrypt+
                          key-number
                          (string-to-unsigned-longs string t)
                          nil
                          product-id
                          nil))

(defun dongle-decrypt-to-string (key-number data &key (product-id *product-id*))
  "Decrypts DATA to a Lisp string using the dongle with the product ID
PRODUCT-ID and the key with the number KEY-NUMBER.  DATA must be a
sequence of integers of type '(UNSIGNED-BYTE 32).  This operation will
obviously only succeed if DATA is \(equivalent to) the result of
DONGLE-ENCRYPT-STRING or TEA-ENCRYPT-STRING with the same key.

The returned string will be STRING= but not identical to the string
that was originally encrypted."
  (unsigned-longs-to-string
   (dongle-decrypt/encrypt +sgl-crypt-mode-decrypt+ key-number data nil product-id nil)))

(defun dongle-encrypt-lisp-object (key-number object &key (product-id *product-id*))
  "Encrypts the Lisp object OBJECT using the dongle with the product
ID PRODUCT-ID and the key with the number KEY-NUMBER.  The return
value is a \(static) array of integers of type \(UNSIGNED-BYTE 32)
which can be \(given the right key) decrypted back to a Lisp object
using DONGLE-DECRYPT-TO-LISP-OBJECT or TEA-DECRYPT-TO-LISP-OBJECT.

Objects are encrypted using a naïve serialization algorithm based on
WRITE-TO-STRING which is neither fast nor space-efficient.
Furthermore, only objects which can be printed readably can be
encrypted this way."
  (dongle-decrypt/encrypt +sgl-crypt-mode-encrypt+ key-number (serialize object) nil product-id nil))

(defun dongle-decrypt-to-lisp-object (key-number data &key (product-id *product-id*))
  "Decrypts DATA to a Lisp object using the dongle with the product ID
PRODUCT-ID and the key with the number KEY-NUMBER.  DATA must be a
sequence of integers of type (UNSIGNED-BYTE 32).  This operation will
obviously only succeed if DATA is \(equivalent to) the result of
DONGLE-ENCRYPT-LISP-OBJECT or TEA-ENCRYPT-LISP-OBJECT with the same
key.

Decrypted numbers, symbols, and characters will be EQL to the objects
originally encrypted.  Other objects will only be EQUAL or EQUALP.
See the docstring of STORE-LISP-OBJECT and the code in
serialize.lisp."
  (unserialize
   (dongle-decrypt/encrypt +sgl-crypt-mode-decrypt+ key-number data nil product-id nil)))

(defun tea-encrypt (key data &key length (copyp t))
  "Encrypts DATA in RAM \(without using a dongle and without accessing
the SG-Lock DLL) with the Tiny Encryption Algorithm using the key KEY.
DATA must either be a sequence of \(UNSIGNED-BYTE 32) integers or an
FLI pointer.  In the latter case, LENGTH is the number of dwords to be
processed.  In both cases, the number of dwords to be processed must
be even.  KEY must be a four-element sequence of \(UNSIGNED-BYTE 32)
integers representing the key to be used.

If DATA is a pointer, the encryption is performed in place and the
pointer is returned, otherwise a static array containing the encrypted
elements from DATA is the return value.  If DATA is a static array of
element type \(UNSIGNED-BYTE 32) and COPYP is NIL, the contents of
DATA will also be modified in place and DATA will be returned.

This is the inverse of TEA-DECRYPT.  See also DONGLE-ENCRYPT."
  (tea-decrypt/encrypt #'tea-encrypt-raw key data length copyp))

(defun tea-decrypt (key data &key length (copyp t))
  "Decrypts DATA in RAM \(without using a dongle and without accessing
the SG-Lock DLL) with the Tiny Encryption Algorithm using the key KEY.
DATA must either be a sequence of \(UNSIGNED-BYTE 32) integers or an
FLI pointer.  In the latter case, LENGTH is the number of dwords to be
processed.  In both cases, the number of dwords to be processed must
be even.  KEY must be a four-element sequence of \(UNSIGNED-BYTE 32)
integers representing the key to be used.

If DATA is a pointer, the encryption is performed in place and the
pointer is returned, otherwise a static array containing the encrypted
elements from DATA is the return value.  If DATA is a static array of
element type \(UNSIGNED-BYTE 32) and COPYP is NIL, the contents of
DATA will also be modified in place and DATA will be returned.

This is the inverse of TEA-ENCRYPT.  See also DONGLE-DECRYPT."
  (tea-decrypt/encrypt #'tea-decrypt-raw key data length copyp))

(defun tea-encrypt-string (key string)
  "Encrypts the Lisp string STRING in RAM \(without using a dongle and
without accessing the SG-Lock DLL) with the Tiny Encryption Algorithm
using the key KEY.  KEY must be a four-element sequence of
\(UNSIGNED-BYTE 32) integers representing the key to be used.

The return value is a \(static) array of integers of type
\(UNSIGNED-BYTE 32) which can be \(given the right key) decrypted back
to a string using TEA-DECRYPT-TO-STRING or DONGLE-DECRYPT-TO-STRING."
  (tea-decrypt/encrypt #'tea-encrypt-raw key (string-to-unsigned-longs string t) nil nil))

(defun tea-decrypt-to-string (key data)
  "Decrypts DATA to a Lisp string in RAM \(without using a dongle and
without accessing the SG-Lock DLL) with the Tiny Encryption Algorithm
using the key KEY.  KEY must be a four-element sequence of
\(UNSIGNED-BYTE 32) integers representing the key to be used.  DATA
must be a sequence of integers of type '(UNSIGNED-BYTE 32).  This
operation will obviously only succeed if DATA is \(equivalent to) the
result of TEA-ENCRYPT-STRING or DONGLE-ENCRYPT-STRING with the same
key.

The returned string will be STRING= but not identical to the string
that was originally encrypted."
  (unsigned-longs-to-string
   (tea-decrypt/encrypt #'tea-decrypt-raw key data nil nil)))

(defun tea-encrypt-lisp-object (key object)
  "Encrypts the Lisp object OBJECT in RAM \(without using a dongle and
without accessing the SG-Lock DLL) with the Tiny Encryption Algorithm
using the key KEY.  KEY must be a four-element sequence of
\(UNSIGNED-BYTE 32) integers representing the key to be used.

The return value is a \(static) array of integers of type
\(UNSIGNED-BYTE 32) which can be \(given the right key) decrypted back
to a Lisp object using TEA-DECRYPT-TO-LISP-OBJECT or
DONGLE-DECRYPT-TO-LISP-OBJECT.

Objects are encrypted using a naïve serialization algorithm based on
WRITE-TO-STRING which is neither fast nor space-efficient.
Furthermore, only objects which can be printed readably can be
encrypted this way."
  (tea-decrypt/encrypt #'tea-encrypt-raw key (serialize object) nil nil))

(defun tea-decrypt-to-lisp-object (key data)
  "Decrypts DATA to a Lisp object in RAM \(without using a dongle and
without accessing the SG-Lock DLL) with the Tiny Encryption Algorithm
using the key KEY.  KEY must be a four-element sequence of
\(UNSIGNED-BYTE 32) integers representing the key to be used.  This
operation will obviously only succeed if DATA is \(equivalent to) the
result of DONGLE-ENCRYPT-LISP-OBJECT or TEA-ENCRYPT-LISP-OBJECT with
the same key.

Decrypted numbers, symbols, and characters will be EQL to the objects
originally encrypted.  Other objects will only be EQUAL or EQUALP.
See the docstring of STORE-LISP-OBJECT and the code in
serialize.lisp."
  (unserialize
   (tea-decrypt/encrypt #'tea-decrypt-raw key data nil nil)))

(defun sign-data (data-pointer length interval key-number key product-id feedback-value)
  "Internal function which signs LENGTH octets starting at \(the FLI
pointer) DATA-POINTER.  See the docstring of SIGN for the other
parameters and the return value of this function."
  (check-type interval (or null (integer 0 #.+max-interval+)))
  (check-type feedback-value (or null (unsigned-byte 64)))
  (unless length
    (simple-dongle-error "No length provided."))
  (when (and interval (null key-number))
    (simple-dongle-error "No key number provided."))
  (when (null key)
    (cond ((or (eql interval 0)
               (and interval (<= length 8)))
           (setq key (load-time-value (make-static-u32-array (list 0 0 0 0)))))
          (t (simple-dongle-error "No key provided."))))
  (unless (= (length key) 4)
    (simple-dongle-error "Key must have length 4."))
  (with-dynamic-lisp-array-pointer (key-pointer (make-static-u32-array key) :type :unsigned-long)  
    (with-dynamic-foreign-objects
        ((feedback-register :unsigned-long
                            :initial-contents (cond (feedback-value
                                                     (list (logand feedback-value #xffffffff)
                                                           (ash feedback-value -32)))
                                                    (t (load-time-value
                                                        (list +sgl-sign-data-initvector-1+
                                                              +sgl-sign-data-initvector-2+))))      ))
      (multiple-value-bind (quotient remainder)
          (floor length 8)
        (when (plusp quotient)
          (sgl-sign-data-comb product-id key-pointer key-number
                              interval (* 8 quotient) data-pointer feedback-register))
        ;; sign the rest of the data which isn't a full 64-bit block
        (when (plusp remainder)
          (let ((rest (allocate-dynamic-foreign-object :type :unsigned-byte :nelems 8)))
            (dotimes (i 8)
              (setf (foreign-typed-aref '(unsigned-byte 8) rest i)
                    (cond ((< i remainder)
                           (foreign-typed-aref '(unsigned-byte 8) data-pointer (+ (* 8 quotient) i)))
                          (t (ldb (byte 8 (* 8 (mod i 4))) +sgl-sign-data-fillupdata+)))))
            (sgl-sign-data-comb product-id key-pointer key-number
                                (and (zerop quotient) interval) 8 rest feedback-register))))
      (+ (dereference feedback-register)
         (ash (dereference feedback-register :index 1) 32)))))

(defgeneric sign (data &key feedback-value interval key-number key product-id &allow-other-keys)
  (:documentation "Signs DATA using the dongle with the product ID
PRODUCT-ID \(the default being *PRODUCT-ID* as usual) and/or the Tiny
Encryption Algorithm in RAM.  DATA can be a sequence \(i.e. a list or
an array) of \(UNSIGNED-BYTE 32) integers or a pathname denoting a
file or an FLI pointer pointing to an area of memory which is to be
signed.  In the last case, the keyword argument LENGTH specifies how
many octets of data are to be signed.

If DATA is a pathname, the whole contents of the file denoted by DATA
will be mapped into memory.  If you don't want that, you will have to
read smaller chunks of the file using READ-SEQUENCE and sign them
using successive calls to SIGN.  The test code contains an example for
this technique - see the file sign.lisp.

If INTERVAL is NIL, neither a dongle nor the SG-Lock DLL is used and
the signing takes place in RAM using the Tiny Encrpytion Algorithm.
Otherwise, INTERVAL must be a non-negative integer N such that
2^\(N+3) is still a fixnum.  If M is 2^N, then each Mth 64-bit block
\(which always include the first one) will be signed using the dongle
while all others will be signed in RAM.  Specifically, if N is 0
\(zero), /all/ the data will be signed using the dongle.

If INTERVAL is not NIL, KEY-NUMBER must be specified to denote which
key of the dongle is to be used for signing.  If INTERVAL is not 0
\(zero), KEY must be specified to denote the key - a four-element
sequence of \(UNSIGNED-BYTE 32) integers - for the in-RAM signing.

The signing process operates on successive 64-bit blocks using the
signature of the previous block as the initial \"feedback\" value.
The signature of the last block is returned by SIGN as the signature
of DATA.  This, together with the fact that the first block is always
signed using the dongle if INTERVAL is not NIL, ensures that the
signature of DATA always depends on the dongle if INTERVAL is an
integer.

If the block of data to be signed is a sequence of octets the length
of which is not divisible by 8, then the function makes sure that the
sequence is extended consistently.  As a technical detail, this 64-bit
\"fill\" block is only ever signed using the dongle if INTERVAL is not
NIL /and/ if DATA consists of less than eight octets.

The return value of SIGN is an integer of type \(UNSIGNED-BYTE 64).

Signing always starts with the same \"feedback\" value unless you
explicitly provide an \(UNSIGNED-BYTE 64) integer FEEDBACK-VALUE to
start with.  This can be utilized to sign larger amounts of data with
several successive calls to SIGN usign the return value of one call as
the \"feedback value\" for the next call.  See also the remark about
the test code above."))

(defmethod sign ((data array) &key
                 feedback-value (interval +max-interval+) key-number key (product-id *product-id*))
  (with-dynamic-lisp-array-pointer (data-pointer (make-static-u32-array data) :type :unsigned-long)
    (sign-data data-pointer (* 4 (length data)) interval key-number key product-id feedback-value)))

(defmethod sign ((data list) &key
                 feedback-value (interval +max-interval+) key-number key (product-id *product-id*))
  (with-dynamic-lisp-array-pointer (data-pointer (make-static-u32-array data) :type :unsigned-long)
    (sign-data data-pointer (* 4 (length data)) interval key-number key product-id feedback-value)))

(defmethod sign ((data pathname) &key
                 feedback-value (interval +max-interval+) key-number key (product-id *product-id*))
  (let ((length (with-open-file (stream data :element-type '(unsigned-byte 8))
                  (file-length stream))))
    (lw-win:with-mapped-file (data-pointer data)
      (sign-data data-pointer length interval key-number key product-id feedback-value))))

(defmethod sign (data &key
                 feedback-value (interval +max-interval+) key-number key (product-id *product-id*) length)
  (check-type data fli-pointer)
  (sign-data data length interval key-number key product-id feedback-value))

(defun sign-lisp-object (object &key
                         feedback-value (interval +max-interval+) key-number key (product-id *product-id*))
  "Signs the Lisp object OBJECT by first serializing it using a naïve
algorithm based on WRITE-TO-STRING and then calling SIGN.  All
parameters except for OBJECT are fed to SIGN."
  (sign (serialize object)
        :interval interval
        :key-number key-number
        :key key
        :product-id product-id
        :feedback-value feedback-value))
  
