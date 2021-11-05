;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/buffer.lisp,v 1.9 2013/01/11 17:14:20 edi Exp $

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

(defmethod initialize-instance :after ((stream odd-stream) &rest initargs)
  "Creates the stream's buffer after the stream \(object) has been created."
  (declare (ignore initargs))
  (declare (optimize speed))
  (with-accessors ((buffer-size odd-stream-buffer-size)
                   (buffer odd-stream-buffer))
      stream
    (setq buffer (make-array buffer-size
                             :element-type 'octet
                             :initial-element 0))))

(defmethod at-buffer-limit-p ((stream odd-stream))
  "Returns true iff the octet position of the buffer is right at the
limit, i.e. if it would be invalidated if it were increased."
  (declare (optimize speed))
  (with-accessors ((octet-position odd-stream-octet-position)
                   (buffer-octet-limit odd-stream-buffer-octet-limit))
      stream
    (>= octet-position buffer-octet-limit)))

(defmethod at-buffer-end-p ((stream odd-stream))
  "Returns true iff the octet position of the buffer is right at the
end of the buffer, i.e. if it would be illegal \(in terms of Lisp
array access) to increase it."
  (declare (optimize speed))
  (with-accessors ((octet-position odd-stream-octet-position)
                   (buffer-size odd-stream-buffer-size))
      stream
    (>= octet-position buffer-size)))

(defmethod fill-buffer ((stream odd-input-stream))
  "Tries to fill as much of the buffer as possible beginning from the
end of the valid part of the buffer."
  (declare (optimize speed))
  (with-accessors ((stream odd-stream-stream)
                   (buffer odd-stream-buffer)
                   (buffer-octet-limit odd-stream-buffer-octet-limit)
                   (octet-position odd-stream-octet-position)
                   (buffer-file-position odd-stream-buffer-file-position))
      stream
    ;; at this point we'll always be at the end of the valid part of
    ;; the "current" buffer; for input streams this is obvious, and
    ;; for IO streams the :AROUND method below which calls
    ;; FLUSH-BUFFER first will take care of that
    (setq buffer-file-position (file-position stream)
          buffer-octet-limit (read-sequence buffer stream)
          octet-position 0)))

(defmethod fill-buffer :around ((stream odd-io-stream))
  "If the stream is an IO stream, i.e. if it's also used for writing,
we have to make sure that the buffer is flushed \(if necessary) before
it's refilled."
  (declare (optimize speed))
  (with-accessors ((buffer-octet-limit odd-stream-buffer-octet-limit)
                   (buffer-bit-limit odd-stream-buffer-bit-limit))
      stream
    (flush-buffer stream)
    (call-next-method)
    ;; if the buffer was really filled, we can get rid of the bit
    ;; limit, otherwise we might have "overlapping bits" - see
    ;; function FLUSH-BUFFER
    (unless (zerop buffer-octet-limit)
      (setq buffer-bit-limit 0))))

(defmethod flush-buffer ((stream odd-output-stream) &key everything re-position)
  "Writes out the valid part of the buffer to the underlying stream.
If EVERYTHING is true, we also write out \"overlapping\" bits at the
end.  \(We usually don't do that in order not to partially overwrite
otherwise valid octets on the disk.)  If RE-POSITION is true, we also
adjust the \"virtual\" position of the buffer towards the end \(this
happens when WRITE-BYTE has reached the end of the buffer), otherwise
we just write but leave the buffer where it is.  In case of
RE-POSITION the buffer is also cleared and invalidated.

RE-POSITION and EVERYTHING must not be used both at the same time."
  (declare (optimize speed))
  (with-accessors ((stream odd-stream-stream)
                   (buffer odd-stream-buffer)
                   (buffer-octet-limit odd-stream-buffer-octet-limit)
                   (buffer-bit-limit odd-stream-buffer-bit-limit)
                   (octet-position odd-stream-octet-position)
                   (buffer-file-position odd-stream-buffer-file-position)
                   (buffer-modified-p odd-stream-buffer-modified-p))
      stream
    ;; do nothing if nothing was modified after the last time the
    ;; buffer was flushed
    (when buffer-modified-p
      ;; re-position to where the buffer is supposed to sit withing
      ;; the underlying stream
      (file-position stream buffer-file-position)
      ;; write out buffer up to (octet) limit
      (write-sequence buffer stream :end buffer-octet-limit)
      ;; now handle potential "overlapping" bits
      (when (and (plusp buffer-bit-limit)
                 (or everything
                     (input-stream-p stream)))
        (let* ((current-file-position (file-position stream))
               ;; if this is an IO stream, we're lucky as we can
               ;; "mask" in the bits into the full octet that
               ;; we grab from the disk
               (next-byte (or (and (input-stream-p stream)
                                   (prog1
                                       (read-byte stream nil)
                                     (file-position stream current-file-position)))
                              0)))
          (setf (ldb (byte buffer-bit-limit 0) next-byte)
                (ldb (byte buffer-bit-limit 0) (aref buffer buffer-octet-limit))
                buffer-bit-limit 0)
          (write-byte next-byte stream)
          (file-position stream current-file-position))))
    (when re-position
      (setq buffer-file-position (file-position stream))
      (let ((unfinished-octet
             ;; remember overlapping bits before clearing buffer
             ;; contents
             (and (plusp buffer-bit-limit)
                  (aref buffer buffer-octet-limit))))
        (fill buffer 0)
        ;; re-enable bits we just remembered
        (when unfinished-octet
          (setf (aref buffer 0) unfinished-octet)))
      (setq octet-position 0))
    (setq buffer-modified-p (plusp buffer-bit-limit))
    (setq buffer-octet-limit 0)))
