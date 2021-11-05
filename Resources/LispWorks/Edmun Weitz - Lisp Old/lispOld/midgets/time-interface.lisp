;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/time-interface.lisp,v 1.17 2013/08/23 08:04:12 edi Exp $

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

(define-interface time-interface ()
  ((caret-pos :initform 0
              :documentation "Keeps track of the current caret
position of TIME-PANE because there's no way to query the pane for
it.")
   (last-caret-pos :initform nil
                   :documentation "Keeps track of the last caret
position of TIME-PANE without text being selected.  Needed to handle
double clicks.")
   (second :initarg :second
        :initform (current-second)
        :accessor time-interface-second
        :documentation "The second part of the time which is currently
shown.")
   (minute :initarg :minute
           :initform (current-minute)
           :accessor time-interface-minute
           :documentation "The minute part of the time which is
currently shown.")
   (hour :initarg :hour
         :initform (current-hour)
         :accessor time-interface-hour
         :documentation "The hour part of the time which is currently
shown.")
   (callback :initarg :callback
             :initform nil
             :accessor time-interface-callback
             :documentation "A function which is called whenever the
time which is shown changes.")
   ;; the previous values for second, minute, and hour as strings -
   ;; kept as fallback values for the case that the user enters
   ;; invalid data
   prev-second
   prev-minute
   prev-hour)
  (:panes
   ;; a text pane which shows the time (and can be used to change it)
   ;; in HH:MM:SS format
   (time-pane
    text-input-pane
    :change-callback 'time-pane-changed
    :text (format-time interface)
    :internal-max-width t)
   ;; a button used to increase the second, minute, or hour - whatever
   ;; is currently selected
   (up-button
    push-button
    :callback-type :interface
    :callback 'time-up-button-pressed
    :image *up-arrow*)
   ;; a button used to decrease the second, minute, or hour - whatever
   ;; is currently selected
   (down-button
    push-button
    :callback-type :interface
    :callback 'time-down-button-pressed
    :image *down-arrow*))
  (:layouts
   (button-layout
    column-layout
    '(up-button down-button)
    :gap 0)
   (main-layout
    row-layout
    '(time-pane button-layout)
    :gap 0
    :internal-max-width t))
  (:documentation "An interface used to present a time and enable the
user to modify it.")
  (:default-initargs
   :layout 'main-layout
   :visible-max-width t
   :create-callback (lambda (interface)
                      (with-slots (caret-pos time-pane
                                   second minute hour
                                   prev-second prev-minute prev-hour)
                          interface
                        (let ((font (find-best-font time-pane
                                                    (make-font-description :stock
                                                                           #+:win32 :windows-menu-font
                                                                           #-:win32 :system-font))))
                          (setf (simple-pane-font time-pane) font
                                ;; initial caret position is at end of string
                                caret-pos (length (text-input-pane-text time-pane))
                                ;; initialize PREV-FOO values
                                prev-second (format nil "~2,'0D" second)
                                prev-minute (format nil "~2,'0D" minute)
                                prev-hour (format nil "~2,'0D" hour)))))))

(defmethod format-time ((interface time-interface))
  "Returns the time currently represented by the time interface
INTERFACE as a string in HH:MM:SS format."
  (with-slots (second minute hour)
      interface
    (format nil "~2,'0D:~2,'0D:~2,'0D"
            hour minute second)))

(defun set-time-pane-selection (interface &key curr-caret-pos start end force which)
  "Sets the selected area of the TIME-PANE of the time interface
INTERFACE to be the one between START and END but only if there's no
colon between START and END.  Otherwise, an area which doesn't contain
a colon but contains the caret is selected.  Also sets the caret
position the interface keeps track of to CURR-CARET-POS unless it can
be inferred that the selection was the result of a double-click in
which case the LAST-CARET-POS of INTERFACE is used.  If FORCE is true,
the selection is always extended to contain two digits by
FIND-INTERVAL.  In this case, the return value is the second value of
FIND-INTERVAL.  If WHICH is true, it is fed to FIND-INTERVAL to
determine which one of the hour, minute, second blocks is to be
selected."
  (with-slots (caret-pos last-caret-pos time-pane)
      interface
    (let* ((text (text-input-pane-text time-pane))
           (length (length text)))
      ;; when the selection comprised the whole text and
      ;; LAST-CARET-POS is not NIL, this is the end of a double-click,
      ;; so we ignore CURR-CARET-POS (which'll always be at the end of
      ;; the string)
      (when (and (zerop start)
                 (= (length text) end)
                 last-caret-pos)
        (setq curr-caret-pos last-caret-pos))
      ;; remember caret position
      (setq caret-pos curr-caret-pos)
      (multiple-value-bind (colon1-pos colon2-pos)
          (colon-positions text)
        (cond ((or force
                   (not (or (<= 0 start end colon1-pos)
                            (<= (1+ colon1-pos) start end colon2-pos)
                            (<= (1+ colon2-pos) start end length))))
               (multiple-value-bind (interval which)
                   (find-interval caret-pos colon1-pos colon2-pos length which)
                 (apply #'set-text-input-pane-selection time-pane interval)
                 which))
              (t (set-text-input-pane-selection time-pane start end)))))))

(defun time-up-button-pressed (interface &optional (delta 1))
  "A callback which is called \(with one argument) when the UP button
in the time interface INTERFACE is pressed.  The DELTA argument
actually determines whether the value is supposed to be incremented or
decremented, so we can use this function for the DOWN button as well."
  (with-slots (last-caret-pos caret-pos second minute hour time-pane callback)
      interface
    ;; set focus back to text pane or otherwise some of the operations
    ;; below will fail
    (set-pane-focus time-pane)
    ;; unset LAST-CARET-POSITION because this is not part of a double
    ;; click in the text pane
    (setq last-caret-pos nil)
    (multiple-value-bind (start end)
        (text-input-pane-selection time-pane)
      ;; call SET-TIME-PANE-SELECTION with :FORCE T to find out what
      ;; to increment/decrement - hours, minutes, or seconds
      (let ((which (set-time-pane-selection interface
                                            :start start
                                            :end end
                                            :curr-caret-pos caret-pos
                                            :force t)))
        ;; helper functions to increase/decrease values and keep track
        ;; of the neighbors and valid results
        (labels ((incf-hour (delta)
                   (incf hour delta)
                   (when (minusp hour)
                     (setq hour 23))
                   (when (> hour 23)
                     (setq hour 0)))
                 (incf-minute (delta)
                   (incf minute delta)
                   (when (minusp minute)
                     (setq minute 59)
                     (incf-hour -1))
                   (when (> minute 59)
                     (setq minute 0)
                     (incf-hour 1)))
                 (incf-second (delta)
                   (incf second delta)
                   (when (minusp second)
                     (setq second 59)
                     (incf-minute -1))
                   (when (> second 59)
                     (setq second 0)
                     (incf-minute 1))))
          (funcall (case which
                     (0 #'incf-hour)
                     (1 #'incf-minute)
                     (otherwise #'incf-second))
                   delta))
        ;; change text and selection of TIME-PANE so that they fit
        ;; with the new values
        (let ((text (format-time interface)))
          (setf (text-input-pane-text time-pane) text)
          (set-time-pane-selection interface
                                   :start start
                                   :end end
                                   :curr-caret-pos caret-pos
                                   :force t
                                   :which which)
          (when callback
            (funcall callback interface)))))))
           
(defun time-down-button-pressed (interface)
  "A callback which is called when the DOWN button in the
time interface INTERFACE is pressed.  See TIME-UP-BUTTON-PRESSED."
  (time-up-button-pressed interface -1))

(defun time-pane-changed (text pane interface curr-caret-pos)
  "A callback which is called whenever the contents of the TIME-PANE
text input pane PANE of the time interface INTERFACE change."
  (with-slots (second minute hour prev-second prev-minute prev-hour caret-pos last-caret-pos callback)
      interface
    ;; remember current caret position
    (setq caret-pos curr-caret-pos)
    (multiple-value-bind (start end)
        (text-input-pane-selection pane)
      ;; when no text is selected, we keep track of the caret position
      ;; in LAST-CARET-POS - this might be the first part of a double-click
      (when (= start end curr-caret-pos)
        (setq last-caret-pos curr-caret-pos))
      ;; if the user has removed colons, we put them back in,
      ;; at the current caret position
      (loop while (< (count #\: text :test #'char=) 2)
            do (setq text (concatenate 'string
                                       (subseq text 0 curr-caret-pos)
                                       ":"
                                       (subseq text curr-caret-pos (length text)))))
      ;; if the user has added, we remove them - we start from the
      ;; current caret position and go backwards
      (loop for pos = (or (position #\: text
                                    :test #'char=
                                    :end curr-caret-pos
                                    :from-end t)
                          (position #\: text :test #'char=))
            while (> (count #\: text :test #'char=) 2)
            do (setq text (concatenate 'string
                                       (subseq text 0 pos)
                                       (subseq text (1+ pos) (length text)))))
      ;; now we remove every character which is illegal
      (setq text
            (coerce (loop for char across text
                          when (find char "0123456789: " :test #'char=)
                          collect char)
                    'string))
      (destructuring-bind (hour-string minute-string second-string)
          (mapcar #'clean-number
                  (split #\: text)
                  '(24 60 60)
                  (list prev-hour prev-minute prev-second))
        ;; now TEXT will be a "cleaned" version of the user input
        (setf text (format nil "~A:~A:~A"
                           hour-string minute-string second-string))
        (setf (text-input-pane-text pane) text
              second (safe-parse-integer second-string)
              minute (safe-parse-integer minute-string)
              hour (safe-parse-integer hour-string)
              prev-second second-string
              prev-minute minute-string
              prev-hour hour-string)
        (set-time-pane-selection interface
                                 :start start
                                 :end end
                                 :curr-caret-pos curr-caret-pos)
        (when callback
          (funcall callback interface))))))

(defun reset-time-interface (interface)
  "This function is called to update the graphical state of the time
interface INTERFACE based on the contents of its SECOND, MINUTE, and
HOUR slots."
  (with-slots (time-pane callback)
      interface
    (setf (text-input-pane-text time-pane)
          (format-time interface))
    (when callback
      (funcall callback interface))))

(defmethod time-interface-time ((interface time-interface))
  "Returns the time currently shown in the time interface INTERFACE as
a universal time with the date set to 1900-01-01."
  (with-slots (second minute hour)
      interface
    (encode-universal-time second minute hour 1 1 1900)))

(defmethod (setf time-interface-time) (time (interface time-interface))
  "Sets the time shown in the time interface INTERFACE to the
universal time TIME.  Day, month, and year are ignored."
  (multiple-value-bind (second minute hour)
      (decode-universal-time time)
    (with-slots ((isecond second)
                 (iminute minute)
                 (ihour hour))
        interface
      (setq isecond second
            iminute minute
            ihour hour))
    (reset-time-interface interface)
    time))

(defmethod (setf time-interface-second) :after (new-value (interface time-interface))
  "Updates graphical state of INTERFACE after the user has changed the
second manually."
  (reset-time-interface interface))

(defmethod (setf time-interface-minute) :after (new-value (interface time-interface))
  "Updates graphical state of INTERFACE after the user has changed the
minute manually."
  (reset-time-interface interface))

(defmethod (setf time-interface-hour) :after (new-value (interface time-interface))
  "Updates graphical state of INTERFACE after the user has changed the
hour manually."
  (reset-time-interface interface))
