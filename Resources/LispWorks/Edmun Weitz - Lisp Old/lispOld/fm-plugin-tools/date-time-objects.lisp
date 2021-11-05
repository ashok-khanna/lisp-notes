;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/date-time-objects.lisp,v 1.11 2010/07/22 09:38:05 edi Exp $

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :fm-plugin-tools)

(defun fm-date-time-set-normalized-date2* (self year month day)
  "Like FM-DATE-TIME-SET-NORMALIZED-DATE2 but with implicit error
checking and conversion to FixPt arguments."
  (let ((err-code (with-fix-pt (year-ptr year)
                    (with-fix-pt (month-ptr month)
                      (with-fix-pt (day-ptr day)
                        (fm-date-time-set-normalized-date2 self year-ptr month-ptr day-ptr))))))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_DateTime_SetNormalizedDate2"
             err-code))))

(defun fm-date-time-set-normalized-time2* (self hour minute second)
  "Like FM-DATE-TIME-SET-NORMALIZED-TIME2 but with implicit error
checking and conversion to FixPt arguments."
  (let ((err-code (with-fix-pt (hour-ptr hour)
                    (with-fix-pt (minute-ptr minute)
                      (with-fix-pt (second-ptr second)
                        (fm-date-time-set-normalized-time2 self hour-ptr minute-ptr second-ptr))))))
    (unless (zerop err-code)
      (error "Got error code ~A while executing FM_DateTime_SetNormalizedTime2"
             err-code))))

(defmacro with-date-time ((ptr) &body body)
  "Executes BODY with PTR bound to a fresh DateTime object.  The
object is guaranteed to be deleted after the execution of BODY."
  `(let (,ptr)
     (unwind-protect
         (progn
           (setq ,ptr (fm-date-time-constructor1))
           ,@body)
       (when ,ptr
         (ignore-errors
           (fm-date-time-delete ,ptr))))))

(defclass date-time-object (fm-object)
  ()
  (:documentation "A DATE-TIME-OBJECT is a Lisp object which is a
proxy for a FileMaker `DateTime' object."))

(defmethod fm-delete ((date-time-object date-time-object))
  "Deletes the DateTime \(C++) object which is proxied by
DATE-TIME-OBJECT."
  (fm-date-time-delete (pointer date-time-object)))

(defmethod get-second ((date-time-object date-time-object))
  "Returns the seconds represented by DATE-TIME-OBJECT."
  (fm-date-time-get-sec (pointer date-time-object)))

(defmethod get-minute ((date-time-object date-time-object))
  "Returns the minutes represented by DATE-TIME-OBJECT."
  (fm-date-time-get-minute (pointer date-time-object)))

(defmethod get-hour ((date-time-object date-time-object))
  "Returns the hours represented by DATE-TIME-OBJECT."
  (fm-date-time-get-hour (pointer date-time-object)))

(defmethod get-day ((date-time-object date-time-object))
  "Returns the day of the month represented by DATE-TIME-OBJECT."
  (fm-date-time-get-day (pointer date-time-object)))

(defmethod get-month ((date-time-object date-time-object))
  "Returns the month represented by DATE-TIME-OBJECT."
  (fm-date-time-get-month (pointer date-time-object)))

(defmethod get-year ((date-time-object date-time-object))
  "Returns the year represented by DATE-TIME-OBJECT."
  (fm-date-time-get-year (pointer date-time-object)))

(defmethod as-second-minute-hour ((date-time-object date-time-object))
  "Returns as three values the seconds, minutes, and hours
represented by DATE-TIME-OBJECT."
  (values (get-second date-time-object)
          (get-minute date-time-object)
          (get-hour date-time-object)))

(define-setf-expander as-second-minute-hour (place &environment env)
  "A writer corresponding to the reader AS-SECOND-MINUTE-HOUR."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (declare (ignore newval setter))
    (with-unique-names (second minute hour)
      (values dummies
              vals
              `(,second ,minute ,hour)
              `(progn
                 (fm-date-time-set-normalized-time2* (pointer ,getter)
                                                     ,hour ,minute, second)
                 (values ,second ,minute ,hour))
              `(as-second-minute-hour ,getter)))))

(defmethod as-day-month-year ((date-time-object date-time-object))
  "Returns as three values the day of the month, month, and year
represented by DATE-TIME-OBJECT."
  (values (get-day date-time-object)
          (get-month date-time-object)
          (get-year date-time-object)))

(define-setf-expander as-day-month-year (place &environment env)
  "A writer corresponding to the reader AS-DAY-MONTH-YEAR."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (declare (ignore newval setter))
    (with-unique-names (day month year)
      (values dummies
              vals
              `(,day ,month ,year)
              `(progn
                 (fm-date-time-set-normalized-date2* (pointer ,getter)
                                                     ,year ,month ,day)
                 (values ,day ,month ,year))
              `(as-day-month-year ,getter)))))

(defmethod as-universal-time ((date-time-object date-time-object))
  "Returns the date and time represented by DATE-TIME-OBJECT as a
Lisp universal time."
  (encode-universal-time (get-second date-time-object)
                         (get-minute date-time-object)
                         (get-hour date-time-object)
                         (get-day date-time-object)
                         (get-month date-time-object)
                         (get-year date-time-object)))

(defmethod (setf as-universal-time) (new-value (date-time-object date-time-object))
  "Sets the date and time represented by DATE-TIME-OBJECT to the
Lisp universal time NEW-VALUE."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time new-value)
    (setf (as-second-minute-hour date-time-object)
          (values second minute hour)
          (as-day-month-year date-time-object)
          (values day month year)))
  new-value)

(defmethod as-time ((date-time-object date-time-object))
  "Returns the time of the day represented by DATE-TIME-OBJECT as
another DATE-TIME-OBJECT."
  (make-date-time-object :time date-time-object))

(defmethod (setf as-time) ((new-value date-time-object) (date-time-object date-time-object) &key)
  "Sets the time of the day represented by DATE-TIME-OBJECT to
the time represented by the DATE-TIME-OBJECT NEW-VALUE."
  (fm-date-time-set-time (pointer date-time-object) (pointer new-value))
  new-value)

(defmethod as-date ((date-time-object date-time-object))
  "Returns the date represented by DATE-TIME-OBJECT as another
DATE-TIME-OBJECT."
  (make-date-time-object :date date-time-object))

(defmethod (setf as-date) ((new-value date-time-object) (date-time-object date-time-object) &key)
  "Sets the date represented by DATE-TIME-OBJECT to the date
represented by the DATE-TIME-OBJECT NEW-VALUE."
  (fm-date-time-set-date (pointer date-time-object) (pointer new-value))
  new-value)

(defun make-date-time-object (&key universal-time time date second minute hour day month year)
  "Creates and returns a new DATE-TIME-OBJECT.  If UNIVERSAL-TIME
is provided, the new object is set to the Lisp universal time
UNIVERSAL-TIME.  If TIME and/or DATE are provided \(themselves
DATE-TIME-OBJECTs), the corresponding parts of the new object are
set accordingly.  Likewise, if some or all values of SECOND,
MINUTE, HOUR, DAY, MONTH, and YEAR are provided, they are used as
expected.  The three different ways to initialize the new object
are mutually exclusive."
  (when universal-time
    (when (or time date second minute hour day month year)
      (error "Don't provide any other keyword argument if you provide UNIVERSAL-TIME.")))
  (when (or time date)
    (when (or second minute hour day month year)
      (error "Don't provide any other keyword argument if you provide TIME and/or DATE."))
    (unless (and (or (null time) (typep time 'date-time-object))
                 (or (null date) (typep time 'date-time-object)))
      (error "TIME and DATE must be NIL or of type DATE-TIME-OBJECT.")))
  (let (ptr)
    (handler-bind
        ((error (lambda (cond)
                  (declare (ignore cond))
                  (when ptr
                    (ignore-errors
                      (fm-date-time-delete ptr))))))
      (setq ptr (fm-date-time-constructor1))
      (let ((date-time (make-instance 'date-time-object :pointer ptr)))
        (cond (universal-time
               (setf (as-universal-time date-time) universal-time))
              ((or time date)
               (when time
                 (setf (as-time date-time) time))
               (when date
                 (setf (as-date date-time) date)))
              ((or second minute hour day month year)
               (when (or second minute hour)
                 (setf (as-second-minute-hour date-time)
                       (values (or second 0) (or minute 0) (or hour 0))))
               (when (or day month year)
                 (setf (as-day-month-year date-time)
                       (values (or day 1) (or month 1) (or year 1900))))))
        date-time))))

(defmethod as-seconds-since-epoch ((date-time-object date-time-object) &optional as-fix-pt-p)
  "Returns the seconds since the Unix `epoch' represented by
DATE-TIME-OBJECT.  If AS-FIX-PT-P is true, the result is a FIX-PT
object."
  (cond (as-fix-pt-p
         (let ((fix-pt-object (make-fix-pt-object)))
           (fm-date-time-get-seconds-since-epoch (pointer date-time-object)
                                                (pointer fix-pt-object))
           fix-pt-object))
        (t
         (with-fix-pt (fix-pt-ptr 0)
           (fm-date-time-get-seconds-since-epoch (pointer date-time-object) fix-pt-ptr)
           (fm-fix-pt-as-long fix-pt-ptr)))))

(defgeneric (setf as-seconds-since-epoch) (new-value date-time-object)
  (:documentation "Sets DATE-TIME-OBJECT to represent a timestamp
corresponding to NEW-VALUE seconds since the Unix `epoch'."))

(defmethod (setf as-seconds-since-epoch) ((new-value integer) (date-time-object date-time-object))
  (with-fix-pt (fix-pt-ptr new-value)
    (fm-date-time-set-seconds-since-epoch (pointer date-time-object) fix-pt-ptr))
  new-value)

(defmethod (setf as-seconds-since-epoch) ((new-value fix-pt-object) (date-time-object date-time-object))
  (fm-date-time-set-seconds-since-epoch (pointer date-time-object) (pointer new-value))
  new-value)

(defmethod as-seconds-since-midnight ((date-time-object date-time-object) &optional as-fix-pt-p)
  "Returns the seconds since midnight represented by the time
part of DATE-TIME-OBJECT.  If AS-FIX-PT-P is true, the result is
a FIX-PT object."
  (cond (as-fix-pt-p
         (let ((fix-pt-object (make-fix-pt-object)))
           (fm-date-time-get-secs-since-midnight (pointer date-time-object)
                                                 (pointer fix-pt-object))
           fix-pt-object))
        (t
         (with-fix-pt (fix-pt-ptr 0)
           (fm-date-time-get-secs-since-midnight (pointer date-time-object) fix-pt-ptr)
           (fm-fix-pt-as-long fix-pt-ptr)))))

(defgeneric (setf as-seconds-since-midnight) (new-value date-time-object)
  (:documentation "Sets the time part of DATE-TIME-OBJECT to
represent NEW-VALUE seconds since midnight."))

(defmethod (setf as-seconds-since-midnight) ((new-value integer) (date-time-object date-time-object))
  (with-fix-pt (fix-pt-ptr new-value)
    (fm-date-time-set-secs-since-midnight (pointer date-time-object) fix-pt-ptr))
  new-value)

(defmethod (setf as-seconds-since-midnight) ((new-value fix-pt-object) (date-time-object date-time-object))
  (fm-date-time-set-secs-since-midnight (pointer date-time-object) (pointer new-value))
  new-value)

