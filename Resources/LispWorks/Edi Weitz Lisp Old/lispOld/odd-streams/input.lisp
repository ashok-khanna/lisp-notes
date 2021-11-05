;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/input.lisp,v 1.11 2013/01/11 17:14:20 edi Exp $

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

(defmethod bits-available ((stream odd-input-stream))
  "Returns the number of bits that can be read from the \"current
 octet\" within the buffer."
  (with-accessors ((buffer-bit-limit odd-stream-buffer-bit-limit)
                   (bit-position odd-stream-bit-position))
      stream
    (- (if (at-buffer-limit-p stream)
         buffer-bit-limit
         8)
       bit-position)))

(defmethod must-fill-p ((stream odd-input-stream) bits-wanted)
  "Returns true if the buffer must be re-filled before BITS-WANTED
more bits can be read."
  ;; we're either at the very end of the buffer, or we're at the limit
  ;; and the number of available bits isn't large enough
  (or (at-buffer-end-p stream)
      (and (at-buffer-limit-p stream)
           (< (bits-available stream)
              bits-wanted))))

(defmethod stream-read-byte ((stream odd-input-stream))
  "Implements reading one byte \(having BYTE-SIZE bits) from the odd stream."
  (declare (optimize speed))
  (with-accessors ((byte-size odd-stream-byte-size)
                   (buffer odd-stream-buffer)
                   (octet-position odd-stream-octet-position)
                   (bit-position odd-stream-bit-position))
      stream
    (let ((byte 0)
          ;; this is the position within BYTE while we loop below
          (byte-bit-position 0))
      (loop
        ;; loop and read in chunks which come from a whole octet
       (when (>= bit-position 8)
         ;; "add carry"
         (setq bit-position 0)
         (incf octet-position))
       (let ((bits-to-read (- byte-size byte-bit-position)))
         (when (must-fill-p stream bits-to-read)
           (fill-buffer stream)
           ;; if filling the buffer didn't help, we're at the end of
           ;; the file
           (when (must-fill-p stream bits-to-read)
             (return-from stream-read-byte :eof)))
         ;; AVAILABLE-BITS is the number of bits we can read from the
         ;; "current octet"
         (let ((available-bits (bits-available stream)))
           (when (<= bits-to-read available-bits)
             ;; all bits we still have to read are within the current
             ;; octet, so we read them, increment the position, and
             ;; then we're done
             (setf (ldb (byte bits-to-read byte-bit-position) byte)
                   (ldb (byte bits-to-read bit-position)
                        (aref buffer octet-position)))
             (incf bit-position bits-to-read)
             (return-from stream-read-byte byte))
           ;; otherwise read the current octet, then increase the
           ;; position within BYTE and continue looping
           (setf (ldb (byte available-bits byte-bit-position) byte)
                 (ldb (byte available-bits bit-position)
                      (aref buffer octet-position)))
           (setf bit-position 0)
           (incf octet-position)
           (incf byte-bit-position available-bits)))))))

(defmethod stream-clear-input ((stream odd-input-stream))
  "Does nothing, which is allowed by the spec."
  (warn 'odd-stream-operation-not-implemented
        :operation 'clear-input
        :stream stream)
  nil)

(defmethod stream-listen ((odd-input-stream odd-input-stream))
  "Calls the corresponding method for the underlying input stream but
first checks if there's still something in the buffer."
  (declare (optimize speed))
  (with-accessors ((stream odd-stream-stream)
                   (byte-size odd-stream-byte-size))
      odd-input-stream
    ;; note that this might return true although maybe only a few bits
    ;; are available instead of a whole byte...
    (or (not (must-fill-p odd-input-stream byte-size))
        (listen stream))))

;; this is necessary due to the way trivial-gray-streams is
;; implemented although FUNDAMENTAL-BINARY-INPUT-STREAM has a default
;; method which does the same
(defmethod stream-read-sequence ((stream odd-input-stream) sequence start end &key)
  ;; there might be some potential for speed improvements here...
  (loop for index from start below end
        for byte = (read-byte stream nil)
        while byte
        do (setf (elt sequence index) byte)
        finally (return index)))
