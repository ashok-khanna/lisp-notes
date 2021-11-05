;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/date-interface.lisp,v 1.11 2013/08/23 08:04:12 edi Exp $

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

(defclass simple-button (drawn-pinboard-object)
  ((text :initarg :text
         :documentation "The text shown by the button.")
   (x-offset :initarg :x-offset
             :documentation "The horizontal distance between the
button's text and its border.")
   (y-offset :initarg :y-offset
             :documentation "The vertical distance between the
button's text and its border."))
  (:documentation "A SIMPLE-BUTTON is a drawn pinboard object used to
simulate small, flat, rectangular push buttons \(which don't
necessarily have an action associated with them)."))

(defclass number-button (simple-button)
  ((number :initform nil
           :accessor number-button-number
           :documentation "The number of this button."))
  (:documentation "A special kind of a SIMPLE-BUTTON which has a
number on it."))

(defun draw-simple-button (pane simple-button text-color background-color &optional highlight-color)
  "Called when a SIMPLE-BUTTON object has to be drawn on the pinboard
layout PANE.  The button is drawn with the text color TEXT-COLOR and
the background BACKGROUND-COLOR.  When HIGHLIGHT-COLOR is not NIL, an
inner rectangle with this color is drawn within the button."
  (with-geometry simple-button
    (draw-rectangle pane
                    %x% %y% %width% %height%
                    :filled t
                    :foreground background-color)
    (when highlight-color
      (draw-rectangle pane
                      (+ %x% 2) (1+ %y%)
                      (- %width% 4) (- %height% 2)
                      :filled t
                      :foreground highlight-color))
    (with-slots (text x-offset y-offset)
        simple-button
      (when text
        (let* ((date-interface (element-interface pane))
               (font (date-interface-font date-interface)))
          ;; position text according to offset values
          (draw-string pane text
                       (+ %x% x-offset)
                       (+ %y% y-offset)
                       :foreground text-color
                       :font font))))))

(defmethod draw-pinboard-object (pane (simple-button simple-button) &key &allow-other-keys)
  "The method to draw a SIMPLE-BUTTON object.  Always uses the
`inactive caption' colors."
  (draw-simple-button pane simple-button
                      (get-inactive-caption-text-color)
                      (get-inactive-caption-color)))

(defmethod draw-pinboard-object (pane (number-button number-button) &key &allow-other-keys)
  "The method to draw a NUMBER-BUTTON object.  Uses default colors for
text and background unless the button is the selected one within the
pane."
  (let* ((background-color (get-window-color))
         (highlight-color (and (eq number-button
                                      (capi-object-property pane :selected))
                               (get-highlight-color)))
         (foreground-color (cond (highlight-color (get-highlight-text-color))
                                 (t (get-window-text-color)))))
    (draw-simple-button pane number-button foreground-color background-color highlight-color)))

(defun select-number-button (layout x y)
  "This function is called by the enclosing pinboard layout's input
model if the mouse is clicked.  We're only interested in clicks on
number buttons."
  (when-let (number-button (pinboard-object-at-position layout x y))
    (when (typep number-button 'number-button)
      ;; note that some number buttons don't have numbers
      ;; and we're not interested in those
      (when-let (day (number-button-number number-button))
        (let ((date-interface (element-interface layout)))
          (setf (slot-value date-interface 'day) day)
          (when-let (callback (date-interface-callback date-interface))
            (funcall callback date-interface)))
        ;; get the old button that was selected (if there was one)
        (let ((previous-number-button (capi-object-property layout :selected)))
          ;; mark this one (the one that has been clicked on) as selected
          (setf (capi-object-property layout :selected) number-button)
          ;; redraw both buttons (which means they'll get new colors)
          (redraw-pinboard-object number-button)
          (when previous-number-button
            (redraw-pinboard-object previous-number-button)))))))

(define-interface date-interface ()
  ((day :initarg :day
        :initform (current-day)
        :accessor date-interface-day
        :documentation "The day part of the date which is currently
shown.")
   (month :initarg :month
          :initform (current-month)
          :accessor date-interface-month
          :documentation "The month part of the date which is
currently shown.")
   (year :initarg :year
         :initform (current-year)
         :accessor date-interface-year
         :documentation "The year part of the date which is currently
shown.")
   (font :accessor date-interface-font
         :documentation "The font used for the simple buttons in the
interface.")
   (callback :initarg :callback
             :initform nil
             :accessor date-interface-callback
             :documentation "A function which is called whenever the
date which is shown changes."))
  (:panes
   ;; a pulldown menu to select the month
   (month-pane
    option-pane
    :selected-item month
    :items (loop for i from 1 to 12
                 collect i)
    :print-function (let ((months (get-month-names)))
                      (lambda (item)
                        (nth (1- item) months)))
    :callback-type :item
    :selection-callback (lambda (item)
                          (setf month item)
                          (reset-date-interface interface))
    :visible-items-count 12
    :internal-max-width t)
   ;; a text pane showing the year
   (year-pane
    text-input-pane
    :change-callback (lambda (text pane interface caret-pos)
                       (let ((old-text (format nil "~A" year)))
                         (unless (equal text old-text)
                           (cond ((= (length text) 3)
                                  ;; if the text length is 3, the user has pressed
                                  ;; the backspace button - we mark the character which
                                  ;; was about to be deleted but don't delete it
                                  (setf (text-input-pane-text pane) old-text)
                                  (set-text-input-pane-selection pane caret-pos (1+ caret-pos)))
                                 (t
                                  ;; otherwise check if the input results in a valid year
                                  (let ((new-year (safe-parse-integer text)))
                                    (unless (and new-year
                                                 (< 1600 new-year 3000))
                                      (beep-pane pane)
                                      (setq new-year year))
                                    (setq year new-year)
                                    (reset-date-interface interface)))))))
    :text (format nil "~A" year)
    :internal-max-width t)
   ;; a button used to increase the year by one
   (up-button
    push-button
    :callback-type :none
    :callback (lambda ()
                (cond ((>= year 3000)
                       (beep-pane up-button))
                      (t (incf year)
                         (reset-date-interface interface))))
    :image *up-arrow*)
   ;; a button used to decrease the year by one
   (down-button
    push-button
    :callback-type :none
    :callback (lambda ()
                (cond ((<= year 1600) (beep-pane down-button))
                      (t (decf year)
                         (reset-date-interface interface))))
    :image *down-arrow*))
  (:layouts
   ;; the pinboard layout shows the day buttons
   (day-layout
    pinboard-layout
    '()
    :display-callback 'display-day-layout
    :vertical-scroll nil
    :horizontal-scroll nil
    :visible-border t
    :draw-pinboard-objects :local-buffer
    :accepts-focus-p nil
    :input-model '(((:button-1 :press) select-number-button)))
   (year-button-layout
    column-layout
    '(up-button down-button)
    :gap 0)
   (year-layout
    row-layout
    '(year-pane year-button-layout)
    :gap 0
    :internal-max-width t)
   (top-layout
    row-layout
    '(month-pane nil year-layout)
    :adjust :center
    :gap 0)
   (main-layout
    column-layout
    '(top-layout day-layout)
    :adjust :center))
  (:documentation "An interface used to present a date and enable the
user to modify it.")
  (:default-initargs
   :layout 'main-layout
   :title "Date Interface"
   :visible-max-width t))

(defun reset-date-interface (interface)
  "This function is called to update the graphical state of the date
interface INTERFACE based on the contents of its DAY, MONTH, and YEAR
slots."
  (with-slots (day month year day-layout month-pane year-pane callback)
      interface
    (setf (choice-selected-item month-pane)
          month
          (text-input-pane-text year-pane)
          (format nil "~A" year)
          day (min day (number-of-days month year))
          (layout-description day-layout)
          (create-buttons day-layout))
    (when callback
      (funcall callback interface))))

(defmethod (setf date-interface-day) :after (new-value (interface date-interface))
  "Updates graphical state of INTERFACE after the user has changed the
day manually."
  (reset-date-interface interface))

(defmethod (setf date-interface-month) :after (new-value (interface date-interface))
  "Updates graphical state of INTERFACE after the user has changed the
month manually."
  (reset-date-interface interface))

(defmethod (setf date-interface-year) :after (new-value (interface date-interface))
  "Updates graphical state of INTERFACE after the user has changed the
year manually."
  (reset-date-interface interface))

(defmethod date-interface-time ((interface date-interface))
  "Returns the date currently shown in the date interface INTERFACE as
a universal time with seconds, minutes, and hours set to zero."
  (with-slots (day month year)
      interface
    (encode-universal-time 0 0 0 day month year)))

(defmethod (setf date-interface-time) (time (interface date-interface))
  "Sets the date shown in the date interface INTERFACE to the
universal time TIME.  Seconds, minutes, and hours are ignored."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time)
    (declare (ignore second minute hour))
    (with-slots ((iday day)
                 (imonth month)
                 (iyear year))
        interface
      (setq iday day
            imonth month
            iyear year))
    (reset-date-interface interface)
    time))

(defun get-month-info (pinboard)
  "Returns a two-element list corresponding to the month which is to
be displayed in the pinboard layout PINBOARD \(the DAY-LAYOUT of a
date interface).  The first element is the day index \(probably
negative) of the first `day' to be displayed in the upper left square
of the pinboard.  The second element is the number of days of the
month."
  (let* ((date-interface (element-interface pinboard))
         (month (date-interface-month date-interface))
         (year (date-interface-year date-interface))
         (time (encode-universal-time 0 0 0 1 month year))
         (first-week-day (nth-value 6 (decode-universal-time time))))
    (list (- (mod (- (get-first-day-of-week)
                     (1- first-week-day))
                  7)
             7)
          (number-of-days month year))))

(defun create-simple-button (pinboard font content x y width height)
  "Returns a SIMPLE-BUTTON object located at position \(X Y) in the
pinboard layout PINBOARD with width WIDTH and height HEIGHT using the
font FONT do display CONTENT \(which will be a character or an
integer) unless it's NIL.  If CONTENT is not a character, the button
returned will be a NUMBER-BUTTON."
  (let ((text (and content (format nil "~A" content)))
        x-offset y-offset)
    (when text
      (multiple-value-bind (left top right bottom)
          (get-string-extent pinboard text font)
        ;; compute offset for text so that it is centered within the button
        (setq x-offset (floor (- width (- right left)) 2)
              y-offset (+ (- top) (floor (- height (- bottom top)) 2)))))
    (let ((button (make-instance 'simple-button
                                 :text text
                                 :x-offset x-offset
                                 :y-offset y-offset
                                 :x x
                                 :y y
                                 :internal-min-width width
                                 :internal-max-width t
                                 :internal-min-height height
                                 :internal-max-height t)))
      (unless (characterp content)
        (change-class button 'number-button)
        (when (numberp content)
          (setf (number-button-number button) content)))
      button)))

(defun create-buttons (pinboard)
  "Creates all 42 number buttons and the seven simple header buttons
for the pinboard layout PINBOARD \(the slot DAY-LAYOUT of a
date interface).  Returns three values - the list of the buttons, the
width of the 7x7 button square, and its height."
  (let* ((date-interface (element-interface pinboard))
         (font (date-interface-font date-interface)))
    ;; compute size of one button based on font
    (multiple-value-bind (left top right bottom)
        (get-string-extent pinboard "30" font)
      (let* ((width (round (* 1.8 (- right left))))
             (height (round (* 1.4 (- bottom top))))
             (day-buttons (loop for day in (get-weekday-chars)
                                for x from 0 by width
                                collect (create-simple-button pinboard font day x 0 width height)))
             (number-buttons (loop with (day-start day-max) = (get-month-info pinboard)
                                   for i below 42
                                   for day from day-start
                                   collect (create-simple-button pinboard font 
                                                                 (and (plusp day)
                                                                      (<= day day-max)
                                                                      day)
                                                                 (* (mod i 7) width)
                                                                 (* (1+ (floor i 7)) height)
                                                                 width height))))
        ;; mark the button corresponding to the day of the interface as selected
        (setf (capi-object-property pinboard :selected)
              (find (date-interface-day date-interface)
                    number-buttons
                    :key #'number-button-number))
        (values (nconc day-buttons number-buttons)
                (* 7 width)
                (* 7 height))))))

(defun display-day-layout (pinboard x y width height)
  "This function is called to display the DAY-LAYOUT pinboard layout
of a date interface."
  ;; this is actually a hack - the function calls the corresponding
  ;; CAPI internal function to do the work and it only makes sure that
  ;; the layout description is populated the first time it's called
  (unless (layout-description pinboard)
    (let ((font (find-best-font pinboard
                                (make-font-description :stock
                                                       #+:win32 :windows-menu-font
                                                       #-:win32 :system-font)))
          (date-interface (element-interface pinboard)))
      (with-slots (month-pane year-pane)
          date-interface
        (setf (date-interface-font date-interface)
              font
              (simple-pane-font month-pane)
              font
              (simple-pane-font year-pane)
              font
              (simple-pane-background pinboard)
              (get-window-color))))
    (multiple-value-bind (buttons width height)
        (create-buttons pinboard)
      (setf (layout-description pinboard) buttons)
      ;; set pinboard to fixed size
      (set-hint-table pinboard (list :visible-min-width width
                                     :visible-max-width t
                                     :visible-min-height height
                                     :visible-max-height t))))
  (capi::pinboard-pane-display pinboard x y width height))

