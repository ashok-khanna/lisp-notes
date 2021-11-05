;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/functions.lisp,v 1.12 2013/08/23 08:04:12 edi Exp $

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

(defun prompt-for-date (message &key time
                                     (day (current-day))
                                     (month (current-month))
                                     (year (current-year))
                                     callback
                                     (ok-text "OK")
                                     (cancel-text "Cancel"))
  "Displays a date interface with the title MESSAGE prompting the user
for a date.  The date entered by the user is returned as a univeral
time with seconds, minutes, and hours set to zero.  The date interface
is initialized with the values DAY, MONTH, YEAR, or with the universal
time TIME which takes precedence over the other three values.  OK-TEXT
is the text on the button which confirms the user input, CANCEL-TEXT
is the text on the button which cancels.  If the users presses the
cancel button, the function returns NIL.  If CALLBACK is not NIL, it
is supposed to be a function of one argument \(the date interface)
which is called whenever the user changes the date."
  (when time
    (let (dummy)
      (declare (ignore dummy))
      (multiple-value-setq (dummy dummy dummy day month year)
          (decode-universal-time time))))
  (let* ((date-interface (make-instance 'date-interface
                                        :callback callback
                                        :day day
                                        :month month
                                        :year year))
         (ok-button (make-instance 'push-button
                                   :default-p t
                                   :text ok-text
                                   :callback-type :none
                                   :callback (lambda ()
                                               (exit-dialog (date-interface-time date-interface)))))
         (cancel-button (make-instance 'push-button
                                       :cancel-p t
                                       :text cancel-text
                                       :callback-type :none
                                       :callback #'abort-dialog))
         (button-layout (make-instance 'row-layout
                                       :description (list ok-button cancel-button)))
         (main-layout (make-instance 'column-layout
                                     :description (list date-interface button-layout)
                                     :adjust :right))
         (dialog (make-instance 'interface
                                :title message
                                :layout main-layout)))
    (display-dialog dialog)))

(defun prompt-for-time (message &key time
                                     (second (current-second))
                                     (minute (current-minute))
                                     (hour (current-hour))
                                     callback
                                     (ok-text "OK")
                                     (cancel-text "Cancel"))
  "Displays a time interface with the title MESSAGE prompting the user
for a time.  The time entered by the user is returned as a univeral
time with day, month, and year set to 1900-01-01.  The time interface
is initialized with the values SECOND, MINUTE, and HOUR, or with the
universal time TIME which takes precedence over the other three
values.  OK-TEXT is the text on the button which confirms the user
input, CANCEL-TEXT is the text on the button which cancels.  If the
users presses the cancel button, the function returns NIL.  If
CALLBACK is not NIL, it is supposed to be a function of one argument
\(the time interface) which is called whenever the user changes the
time."
  (when time
    (multiple-value-setq (second minute hour)
        (decode-universal-time time)))
  (let* ((time-interface (make-instance 'time-interface
                                        :callback callback
                                        :second second
                                        :minute minute
                                        :hour hour))
         (ok-button (make-instance 'push-button
                                   :default-p t
                                   :text ok-text
                                   :callback-type :none
                                   :callback (lambda ()
                                               (exit-dialog (time-interface-time time-interface)))))
         (cancel-button (make-instance 'push-button
                                       :cancel-p t
                                       :text cancel-text
                                       :callback-type :none
                                       :callback #'abort-dialog))
         (button-layout (make-instance 'row-layout
                                       :description (list ok-button cancel-button)))
         (main-layout (make-instance 'column-layout
                                     :description (list time-interface button-layout)
                                     :adjust :right))
         (dialog (make-instance 'interface
                                :title message
                                :layout main-layout)))
    (display-dialog dialog)))

(defun prompt-for-date-and-time (message &key time
                                              (second (current-second))
                                              (minute (current-minute))
                                              (hour (current-hour))
                                              (day (current-day))
                                              (month (current-month))
                                              (year (current-year))
                                              callback
                                              (date-title "Date")
                                              (time-title "Time")
                                              (ok-text "OK")
                                              (cancel-text "Cancel"))
  "Displays a date and a time interface with the title MESSAGE
prompting the user for date and time.  The date and time entered by
the user are returned as a univeral time.  The interfaces are
initialized with the values SECOND, MINUTE, HOUR, DAY, MONTH, and
YEAR, or with the universal time TIME which takes precedence over the
other six values.  DATE-TITLE is the title for the date interface,
TIME-TITLE is the title for the time interface.  OK-TEXT is the text
on the button which confirms the user input, CANCEL-TEXT is the text
on the button which cancels.  If the users presses the cancel button,
the function returns NIL.  If CALLBACK is not NIL, it is supposed to
be a function of one argument \(the date interface or the time
interface) which is called whenever the user changes the date or the
time.  The callback must be able to distinguish between the date
interface and the time interface."
  (when time
    (multiple-value-setq (second minute hour day month year)
        (decode-universal-time time)))
  (let* ((date-interface (make-instance 'date-interface
                                        :title date-title
                                        :title-position :frame
                                        :callback callback
                                        :day day
                                        :month month
                                        :year year))
         (time-interface (make-instance 'time-interface
                                        :title time-title
                                        :title-position :frame
                                        :callback callback
                                        :second second
                                        :minute minute
                                        :hour hour))
         (ok-button (make-instance 'push-button
                                   :default-p t
                                   :text ok-text
                                   :callback-type :none
                                   :callback (lambda ()
                                               (with-slots (second minute hour)
                                                   time-interface
                                                 (with-slots (day month year)
                                                     date-interface
                                                   (exit-dialog 
                                                    (encode-universal-time second minute hour
                                                                           day month year)))))))
         (cancel-button (make-instance 'push-button
                                       :cancel-p t
                                       :text cancel-text
                                       :callback-type :none
                                       :callback #'abort-dialog))
         (row-layout (make-instance 'row-layout
                                    :adjust :bottom
                                    :description (list time-interface ok-button cancel-button)))
         (main-layout (make-instance 'column-layout
                                     :description (list date-interface row-layout)
                                     :adjust :left))
         (dialog (make-instance 'interface
                                :title message
                                :layout main-layout)))
    (display-dialog dialog)))
