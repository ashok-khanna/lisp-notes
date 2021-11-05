;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/collecting-display-pane.lisp,v 1.6 2013/08/23 08:04:12 edi Exp $

;;; Copyright (c) 2006-2013, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :midgets)

(defclass display-pane-stream (stream:buffered-stream)
  ((pane :initarg :pane
         :reader display-pane-stream-pane
         :documentation "The collecting display pane which is the
target of this stream.")
   (force-output-p :initform nil
                   :initarg :force-output-p
                   :accessor display-pane-stream-force-output-p
                   :documentation "Whether FORCE-OUTPUT should be
called \(i.e. whether the streams's buffer should be flushed)
automatically after each character written \(T), after each #\Newline
\(:NEWLINE), or not at all \(NIL)."))
  (:documentation "The stream class used by collecting display panes,
i.e. streams of this class are character output streams which write
their output to a CAPI display pane.  The streams are implemented on
top of STREAM:BUFFERED-STREAM and holds a reference to the pane they
write to."))

(defclass collecting-display-pane (display-pane)
  ((lock :initform (mp:make-lock :name (format nil "collecting-display-pane-lock-~A"
                                               (incf *collecting-pane-lock-counter*)))
         :reader collecting-display-pane-lock
         :documentation "The lock which is internally used to
synchronize primitive stream operations.  You can use it yourself to
wrap it around higher-order output functions like FORMAT.")
   (auto-scroll-p :initform nil
                  :initarg :auto-scroll-p
                  :accessor collecting-display-pane-auto-scroll-p
                  :documentation "Whether the pane should
automatically scroll to the bottom each time the stream buffer is
flushed.  Make sure your pane /can/ scroll verticall if you set this
slot to a true value.")
   (stream :reader collecting-display-pane-stream
           :documentation "The stream \(of type DISPLAY-PANE-STREAM)
that's associated with this pane."))
  (:documentation "A display pane which has a stream associated with
it that writes to the pane, i.e. whenever you send output to the
stream it ends up in the display pane.  Otherwise, this is simply a
subclass of CAPI:DISPLAY-PANE.")
  (:default-initargs
   :horizontal-scroll t
   :vertical-scroll t
   ;; you can overwrite this, but it doesn't make much sense to have a
   ;; fixed size here
   :visible-min-width nil
   :visible-max-width nil
   :visible-min-height nil
   :visible-max-height nil))
  
(defmethod initialize-instance :after ((pane collecting-display-pane) &rest initargs &key force-output-p)
  "Sets up the stream for the pane.  Declares, as a side effect,
:FORCE-OUTPUT-P to be a valid initarg for collecting display panes and
uses it for the stream."
  (declare (ignore initargs))
  (setf (slot-value pane 'stream)
        (make-instance 'display-pane-stream
                       :pane pane
                       :force-output-p force-output-p
                       :direction :output
                       :element-type 'lw:simple-char)))

(defmethod stream-element-type ((stream display-pane-stream))
  "One of the few methods that have to be specialized for subclasses
of buffered streams.  We always us 'LW:SIMPLE-CHAR as the element
type."
  'lw:simple-char)

(defmethod display-pane-stream-lock ((stream display-pane-stream))
  "`Trampoline' accessor from the stream to its associated pane to get
at the lock they share."
  (collecting-display-pane-lock (display-pane-stream-pane stream)))

(defmacro with-stream-lock (display-pane-stream &body body)
  "Internal utility macro which wrap BODY with WITH-LOCK using the
lock that belongs to the stream."
  `(mp:with-lock ((display-pane-stream-lock ,display-pane-stream))
     ,@body))

(defmethod stream:stream-write-buffer ((stream display-pane-stream) buffer start end)
  "The main working horse of this stream implementation.  Appends the
contents of BUFFER from START to END to the text of the display pane
associated with STREAM.  Takes care of scrolling if needed and makes
sure things happen in the correct thread."
  ;; use lock - strange things can happen otherwise...
  (with-stream-lock stream
    (let ((pane (display-pane-stream-pane stream))
          done)
      ;; one could think about using adjustable arrays if this is not
      ;; fast enough, but for now we shy away from premature optimizations
      (flet ((write-buffer ()
               (let* ((pane-text (display-pane-text pane))
                      (pane-text-length (length pane-text))
                      (new-text (make-string (+ pane-text-length (- end start))
                                             :element-type 'lw:simple-char)))
                 (setf (subseq new-text 0 pane-text-length) pane-text
                       (subseq new-text pane-text-length) (subseq buffer start end)
                       (display-pane-text pane) new-text)
                 (when (collecting-display-pane-auto-scroll-p pane)
                   (scroll pane :vertical :move :end))
                 (setq done t))))
        ;; do the actual work in the correct thread
        (apply-in-pane-process pane #'write-buffer)
        ;; wait until we're finished
        (mp:process-wait (format nil "Waiting on stream ~S" stream) (lambda () done))))))

(defmethod maybe-force-output ((stream display-pane-stream) newline-seen-p)
  "Internal utility function which calls FORCE-OUTPUT on STREAM
depending on its FORCE-OUTPUT-P slot and on whether a #\Newline was
seen in the last output operation."
  (with-slots (force-output-p)
      stream
    (when (or (eq force-output-p t)
              (and newline-seen-p
                   (eq force-output-p :newline)))
      (force-output stream))))

(defmethod stream:stream-write-char :around ((stream display-pane-stream) char)
  "Protects the underlying default implementation of this generic
function with a lock."
  (declare (ignore char))
  (with-stream-lock stream
    (call-next-method)))

(defmethod stream:stream-write-char :after ((stream display-pane-stream) char)
  "Calls FORCE-OUTPUT if requested after the character has been output."
  (maybe-force-output stream (char= char #\Newline)))

(defmethod stream:stream-write-string :around ((stream display-pane-stream) string &optional start end)
  "Protects the underlying default implementation of this generic
function with a lock."
  (declare (ignore string start end))
  (with-stream-lock stream
    (call-next-method)))

(defmethod stream:stream-write-string :after ((stream display-pane-stream) string &optional start end)
  "Calls FORCE-OUTPUT if requested after the string has been output."
  (maybe-force-output stream (find #\Newline string :start start :end end :test #'char=)))

(defmethod stream:stream-write-sequence :around ((stream display-pane-stream) sequence start end)
  "Protects the underlying default implementation of this generic
function with a lock."
  (declare (ignore sequence start end))
  (with-stream-lock stream
    (call-next-method)))

(defmethod stream:stream-write-sequence :after ((stream display-pane-stream) sequence start end)
  "Calls FORCE-OUTPUT if requested after the sequence has been output."
  (maybe-force-output stream (find #\Newline sequence :start start :end end :test #'char=)))


