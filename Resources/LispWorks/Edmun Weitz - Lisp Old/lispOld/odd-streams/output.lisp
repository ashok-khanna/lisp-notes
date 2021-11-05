;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/output.lisp,v 1.12 2013/01/11 17:14:20 edi Exp $

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

(defmethod stream-write-byte ((stream odd-output-stream) byte)
  "Implements writing one byte \(having BYTE-SIZE bits) to the odd stream."
  (declare (optimize speed))
  (with-accessors ((byte-size odd-stream-byte-size)
                   (buffer odd-stream-buffer)
                   (buffer-octet-limit odd-stream-buffer-octet-limit)
                   (buffer-bit-limit odd-stream-buffer-bit-limit)
                   (buffer-size odd-stream-buffer-size)
                   (octet-position odd-stream-octet-position)
                   (bit-position odd-stream-bit-position)
                   (buffer-modified-p t))
      stream
    (flet ((increment-bit-position (increment)
             "Increments the position within the buffer by INCREMENT bits."
             (incf bit-position increment)
             (when (>= bit-position 8)
               ;; "add carry"
               (incf octet-position)
               (setq bit-position (- bit-position 8))
               (when (> octet-position buffer-octet-limit)
                 ;; we reached the buffer's limit and have to increase it
                 (setq buffer-octet-limit octet-position
                       buffer-bit-limit bit-position)))
             ;; if we're at the octet limit we may have to increase
             ;; the bit limit as well
             (when (= buffer-octet-limit octet-position)
               (setq buffer-bit-limit (max buffer-bit-limit bit-position)))))
      ;; this is the position within BYTE while we loop below
      (let ((byte-bit-position 0))
        ;; loop and write in chunks which fit into a whole octet
        (loop
          ;; adjust bit position
         (increment-bit-position 0)
         ;; flush buffer if we're at the end, then re-position because
         ;; we want to continue writing
         (when (>= octet-position buffer-size)
           (flush-buffer stream :re-position t))
         ;; mark buffer as modified
         (setq buffer-modified-p t)
         ;; REMAINING-BITS is the number of bits we can write into the
         ;; "current octet"
         (let ((remaining-bits (- 8 bit-position))
               (bits-to-write (- byte-size byte-bit-position)))
           (when (<= bits-to-write remaining-bits)
             ;; all bits we still have to write fit into the current
             ;; octet, so we write them, increment the position, and
             ;; then we're done
             (setf (ldb (byte bits-to-write bit-position)
                        (aref buffer octet-position))
                   (ldb (byte bits-to-write byte-bit-position) byte))
             (increment-bit-position bits-to-write)
             (return-from stream-write-byte byte))
           ;; otherwise fill the current octet, then increase the
           ;; position within BYTE and continue looping
           (setf (ldb (byte remaining-bits bit-position)
                      (aref buffer octet-position))
                 (ldb (byte remaining-bits byte-bit-position) byte))
           (increment-bit-position remaining-bits)
           (incf byte-bit-position remaining-bits)))))))

(defmethod stream-clear-output ((stream odd-output-stream))
  "Does nothing, which is allowed by the spec."
  (warn 'odd-stream-operation-not-implemented
        :operation 'clear-output
        :stream stream)
  nil)

(defmethod stream-finish-output ((odd-output-stream odd-output-stream))
  "Simply calls the corresponding method for the underlying output
stream after flushing the buffer.  Note that this might fail to
\"flush overlapping bits\"."
  (declare (optimize speed))
  (flush-buffer odd-output-stream)
  (with-accessors ((stream odd-stream-stream))
      odd-output-stream
    (finish-output stream)))

(defmethod stream-force-output ((odd-output-stream odd-output-stream))
  "Simply calls the corresponding method for the underlying output
stream after flushing the buffer.  Note that this might fail to
\"flush overlapping bits\"."
  (declare (optimize speed))
  ;; it's not totally clear if we should flush the buffer here, but
  ;; what else can we do?
  (flush-buffer odd-output-stream)
  (with-accessors ((stream odd-stream-stream))
      odd-output-stream
    (force-output stream)))

(defmethod close :before ((stream odd-output-stream) &key abort)
  "Makes sure the buffer is flushed \(unless ABORT is true) before the
stream is actually closed."
  (declare (optimize speed))
  (unless abort
    (flush-buffer stream :everything t)))

;; this is necessary due to the way trivial-gray-streams is
;; implemented although FUNDAMENTAL-BINARY-OUTPUT-STREAM has a default
;; method which does the same
(defmethod stream-write-sequence ((stream odd-output-stream) sequence start end &key)
  "Writes a sequence to the stream by writing each byte in turn."
  ;; there might be some potential for speed improvements here...
  (loop for index from start below end
        do (write-byte (elt sequence index) stream)
        finally (return sequence)))
