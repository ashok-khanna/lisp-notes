;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/stream.lisp,v 1.13 2013/01/11 17:14:20 edi Exp $

;;; Copyright (c) 2007-2013, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :odd-streams)

(defclass odd-stream (trivial-gray-stream-mixin)
  ((stream :initarg :stream
           :reader odd-stream-stream
           :documentation "The actual stream that's used for input
and/or output - a binary stream with element-type OCTET.")
   (byte-size :initarg :byte-size
              :reader odd-stream-byte-size
              :documentation "The size of the \"apparent element
type\" of the odd stream in bits, i.e. the stream behaves as if its
element type were \(UNSIGNED-BYTE BYTE-SIZE).")
   (buffer :reader odd-stream-buffer
           :documentation "Odd streams are buffered streams, and this
is the buffer - an array of BUFFER-SIZE octets.")
   (buffer-size :initarg :buffer-size
                :reader odd-stream-buffer-size
                :documentation "The size of the stream's buffer in octets.")
   (buffer-octet-limit :initform 0
                       :accessor odd-stream-buffer-octet-limit
                       :documentation "The index of the first octet in
the buffer which isn't valid anymore - but see BUFFER-BIT-LIMIT.")
   (buffer-bit-limit :initform 0
                     :accessor odd-stream-buffer-bit-limit
                     :documentation "If this value is not 0 \(zero),
then the first BUFFER-BIT-LIMIT bits of the buffer element at index
BUFFER-OCTET-LIMIT are valid bits of the buffer.  This value can only
be non-zero for output streams.")
   (octet-position :initform 0
                   :accessor odd-stream-octet-postion
                   :documentation "The current octet position within
the buffer - the actual position is a combination of this value and
BIT-POSITION.")
   (bit-position :initform 0
                 :accessor odd-stream-bit-postion
                 :documentation "The current bit position within the
buffer - the actual position is a combination of this value and
OCTET-POSITION.")
   (buffer-file-position :initform 0
                         :accessor odd-stream-buffer-file-position
                         :documentation "Where the beginning of the
buffer is located within the original stream - measured in octets."))
  (:documentation "A ODD-STREAM object is a stream that's `layered'
atop an existing binary stream in order to allow for `odd' element
types like \(UNSIGNED-BYTE 3).  Odd streams use a buffer that acts as
a \"window\" into the original stream.  Only the part of the buffer
from its beginning up to the bit specified by BUFFER-OCTET-LIMIT and
BUFFER-BIT-LIMIT is valid.

ODD-STREAM itself is a mixin and should not be instantiated."))

(defmethod stream-element-type ((stream odd-stream))
  "Returns the element type of STREAM which is computed from the
BYTE-SIZE slot."
  (declare (optimize speed))
  (with-accessors ((byte-size odd-stream-byte-size))
      stream
    `(unsigned-byte ,byte-size)))

(defmethod close ((stream odd-stream) &key abort)
  "Closes the odd stream by closing the underlying `real' stream."
  (declare (optimize speed))
  (with-accessors ((stream odd-stream-stream))
      stream
    (cond ((open-stream-p stream)
           (close stream :abort abort))
          (t nil))))

(defmethod open-stream-p ((stream odd-stream))
  "An odd stream is open if its underlying stream is open."
  (declare (optimize speed))
  (open-stream-p (odd-stream-stream stream)))

;; note that because of the way the trivial-gray-streams library is
;; implemented, the order of the superclasses matters
(defclass odd-output-stream (odd-stream fundamental-binary-output-stream)
  ((buffer-modified-p :initform nil
                      :accessor odd-stream-buffer-modified-p
                      :documentation "A boolean indicating whether the
stream's buffer has been modified since it was last flushed."))
  (:documentation "An ODD-OUTPUT-STREAM is an ODD-STREAM that can
actually be instatiated and used for output.  Don't use MAKE-INSTANCE
to create a new ODD-OUTPUT-STREAM but use MAKE-ODD-STREAM instead."))

#+:cmu
(defmethod input-stream-p ((stream odd-output-stream))
  "Explicitly states whether this is an input stream.  Only needed for
CMUCL."
  (declare (optimize speed))
  nil)

;; note that because of the way the trivial-gray-streams library is
;; implemented, the order of the superclasses matters
(defclass odd-input-stream (odd-stream fundamental-binary-input-stream)
  ()
  (:documentation "An ODD-INPUT-STREAM is an ODD-STREAM that can
actually be instatiated and used for input.  Don't use MAKE-INSTANCE
to create a new ODD-INPUT-STREAM but use MAKE-ODD-STREAM instead."))

#+:cmu
(defmethod output-stream-p ((stream odd-input-stream))
  "Explicitly states whether this is an output stream.  Only needed
for CMUCL."
  (declare (optimize speed))
  nil)

(defclass odd-io-stream (odd-input-stream odd-output-stream)
  ()
  (:documentation "An ODD-IO-STREAM is an ODD-STREAM that can actually
be instatiated and used for input and output.  Don't use MAKE-INSTANCE
to create a new ODD-IO-STREAM but use MAKE-ODD-STREAM instead."))

#+:cmu
(defmethod input-stream-p ((stream odd-io-stream))
  "Explicitly states whether this is an input stream.  Only needed for
CMUCL."
  (declare (optimize speed))
  t)

#+:cmu
(defmethod output-stream-p ((stream odd-io-stream))
  "Explicitly states whether this is an output stream.  Only needed
for CMUCL."
  (declare (optimize speed))
  t)

(defun make-odd-stream (stream &key (byte-size 1) buffer-size)
  "Creates and returns a new odd stream.  STREAM must be an open
binary stream capable of reading/writing octets.  The resulting odd
stream is an input stream if and only if STREAM is an input stream.
Likewise, it's an output stream if and only if STREAM is an output
stream.  BYTE-SIZE is the size of one stream element in bits, i.e. the
stream will \(try to) behave as if it had element type \(UNSIGNED-BYTE
BYTE-SIZE).

BUFFER-SIZE is the size of the stream's buffer in octets."
  (unless (and (streamp stream)
               (open-stream-p stream))
    (odd-stream-error "~S should have been an open stream." stream))
  (unless (typep byte-size `(integer 1 ,+max-byte-size+))
    (odd-stream-error "The bit size must be a positive integer not greater than ~A."
                      +max-byte-size+))
  ;; complicated computation of default buffer size - is this reasonable?
  (let ((needed-buffer-size (ceiling (* byte-size +recommended-bytes-per-buffer+) 8))
        (max-buffer-size (* +recommended-bytes-per-buffer+ +max-byte-size+)))
    (unless buffer-size
      (setq buffer-size (* +recommended-octets-per-buffer+
                           (ceiling (min (max needed-buffer-size +min-buffer-size+)
                                         max-buffer-size)
                                    +recommended-octets-per-buffer+))))
    (unless (typep buffer-size '(integer 1 *))
      (odd-stream-error "The buffer size must be a positive integer."))
    (unless (>= buffer-size needed-buffer-size)
      (odd-stream-error "The buffer must be large enough to hold at least ~A bytes."
                        +recommended-bytes-per-buffer+))
    (unless (>= buffer-size +min-buffer-size+)
      (odd-stream-error "The buffer should hold at least ~A octets."
                        +min-buffer-size+))
    (unless (<= buffer-size max-buffer-size)
      (odd-stream-error "The buffer must not be larger than ~A octets."
                        max-buffer-size)))
  (make-instance ;; actual type depends on STREAM
                 (cond ((and (input-stream-p stream)
                             (output-stream-p stream))
                        'odd-io-stream)
                       ((input-stream-p stream)
                        'odd-input-stream)
                       ((output-stream-p stream)
                        'odd-output-stream))
                 :stream stream
                 :byte-size byte-size
                 :buffer-size buffer-size))

(defmacro with-open-odd-stream ((var stream
                                     &rest rest
                                     &key byte-size buffer-size)
                                &body body)
  "Like CL:WITH-OPEN-STREAM, but for odd streams.  See MAKE-ODD-STREAM
for the meaning of the parameters."
  ;; parameters are only listed so the IDE can show them
  (declare (ignore byte-size buffer-size))
  `(with-open-stream (,var (make-odd-stream ,stream ,@rest))
     ,@body))

(defmacro with-open-odd-file ((stream filespec
                                      &rest rest
                                      &key byte-size buffer-size direction if-exists if-does-not-exist)
                              &body body)
  "Like CL:WITH-OPEN-FILE, but for odd streams.  See MAKE-ODD-STREAM
for the meaning of the parameters."
  ;; parameters are only listed so the IDE can show them
  (declare (ignore byte-size buffer-size direction if-exists if-does-not-exist))
  (let ((original-stream-var (gensym)))
    `(with-open-file (,original-stream-var ,filespec
                                           :element-type 'octet
                                           ,@(sans rest :byte-size :buffer-size))
       (with-open-odd-stream (,stream ,original-stream-var
                                      ,@(sans rest :direction :if-exists :if-does-not-exist))
         ,@body))))

(defmethod odd-file-length ((odd-stream odd-stream))
  "Like CL:FILE-LENGTH, but using the stream's BYTE-SIZE as its unit
of measurement.  This is an exported function as it can't be done with
Gray streams."
  (declare (optimize speed))
  (with-accessors ((stream odd-stream-stream)
                   (byte-size odd-stream-byte-size)
                   (buffer-octet-limit odd-stream-buffer-octet-limit)
                   (buffer-bit-limit odd-stream-buffer-bit-limit)
                   (buffer-file-position odd-stream-buffer-file-position))
      odd-stream
    ;; compute from the length of the underlying file /or/ from the
    ;; limit of the buffer, whichever is bigger
    (floor (max (* 8 (file-length stream))
                (+ (* 8 (+ buffer-file-position
                           buffer-octet-limit))
                   buffer-bit-limit))
           byte-size)))

(defmethod stream-file-position ((stream odd-stream))
  "Implements the one-argument version of FILE-POSITION.  Not
available on all Common Lisps with Gray streams."
  (declare (optimize speed))
  (with-accessors ((buffer-file-position odd-stream-buffer-file-position)
                   (octet-position odd-stream-octet-position)
                   (bit-position odd-stream-bit-position)
                   (byte-size odd-stream-byte-size))
      stream
    ;; compute from position in buffer and position of buffer in
    ;; original stream
    (/ (+ (* 8 (+ buffer-file-position
                  octet-position))
          bit-position)
       byte-size)))
    
(defmethod (setf stream-file-position) (position-spec (odd-stream odd-stream))
  "Implements the two-argument version of FILE-POSITION.  Not
available on all Common Lisps with Gray streams."
  (declare (optimize speed))
  (with-accessors ((stream odd-stream-stream)
                   (byte-size odd-stream-byte-size)
                   (buffer odd-stream-buffer)
                   (buffer-octet-limit odd-stream-buffer-octet-limit)
                   (buffer-bit-limit odd-stream-buffer-bit-limit)
                   (octet-position odd-stream-octet-position)
                   (bit-position odd-stream-bit-position)
                   (buffer-file-position odd-stream-buffer-file-position))
      odd-stream
    (let ((desired-position
           (case position-spec
             (:start 0)
             (:end (odd-file-length odd-stream))
             (otherwise position-spec))))
      (unless (typep desired-position '(integer 0 *))
        (error 'odd-stream-position-spec-error
               :stream odd-stream
               :position-spec desired-position))
      (let ((desired-position-in-bits (* byte-size desired-position))
            (buffer-file-position-in-bits (* 8 buffer-file-position)))
        (cond ((<= buffer-file-position-in-bits
                   desired-position-in-bits
                   (* 8 (+ buffer-file-position
                           buffer-octet-limit)))
               ;; the new position is still within the valid part of
               ;; the buffer, so we just re-compute the "pointer" into
               ;; the buffer
               (let ((desired-bit-offset (- desired-position-in-bits buffer-file-position-in-bits)))
                 (setq octet-position (floor desired-bit-offset 8)
                       bit-position (mod desired-bit-offset 8))))
              ;; the new position is not within the buffer, so we have
              ;; to re-position the buffer
              (t (when (output-stream-p odd-stream)
                   ;; flush the buffer if necessary
                   (flush-buffer odd-stream :everything t))
                 (let ((desired-file-position (floor desired-position-in-bits 8))
                       (desired-bit-offset (mod desired-position-in-bits 8)))
                   ;; re-position within original stream
                   (file-position stream desired-file-position)
                   ;; clear buffer contents
                   (fill buffer 0)
                   ;; try to re-fill if possible
                   (when (input-stream-p odd-stream)
                     (fill-buffer odd-stream))
                   (setq bit-position desired-bit-offset)
                   (when (zerop buffer-octet-limit)
                     ;; set bit limit only if buffer couldn't be filled
                     (setq buffer-bit-limit desired-bit-offset))))))
      desired-position)))
