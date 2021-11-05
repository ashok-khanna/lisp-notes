;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PLUGIN-EXAMPLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/plugin-example/functions.lisp,v 1.9 2010/07/22 09:38:08 edi Exp $

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

(in-package :plugin-example)

;;; The first eight functions monkey those of the example plug-in
;;; that comes with FileMaker Pro Advanced.

;; returns the version string
(define-plugin-function "Version"
    ()
  (version-string))

;; adds two numbers - same as + operator in FM
(define-plugin-function "Add( number1; number2 )"
    ((number1 :float) (number2 :float))
  (+ number1 number2))

;; appends any positive number of strings - same as & operator in FM
(define-plugin-function "Append ( textToAppend... )"
    ;; we want at least one argument
    ((first :text) &rest (rest :text))
  ;; note that the character styles of the first argument are
  ;; preserved - compare with FileMaker's own XMpl_Append example
  (dolist (arg rest)
    (append-text first arg))
  first)

;; evaluates the argument - same as `Evaluate' function in FM
(define-plugin-function ("Evaluate ( calcToEvaluate )" :result-type :void)
    ;; you could as well use :TEXT here - would be less internal work 
    ((calc :string))
  (evaluate calc *results*))

;; starts the script named SCRIPTNAME on the file specified by FILENAME
(define-plugin-function ("StartScript ( filename; scriptname )" :result-type :void)
    ((filename :string) (scriptname :string))
  (start-script filename scriptname))

;; returns NUMBER in bank-check format, can handle more than twelve
;; digits - compare with original XMpl_NumToWords example
(define-plugin-function "NumToWords ( number )"
    ;; note that we actually want a string here, not a number
    ((number :string))
  (block num-to-words
    (let ((filtered-number (filter-number number t)))
      (when (zerop (length filtered-number))
        (return-from num-to-words ""))
      (let* ((cleaned-number (ensure-two-decimal-places filtered-number))
             (point-pos (position *decimal-separator* cleaned-number))
             (cents (parse-integer cleaned-number :start (1+ point-pos)))
             (dollars (or (parse-integer cleaned-number :end point-pos :junk-allowed t) 0))
             ;; let FORMAT do all the hard work... :)
             (cents-text (format nil "~R Cent~:P" cents))
             (dollars-text (format nil "~R Dollar~:P" dollars)))
        (flet ((normalize-text (string)
                 "Converts the results of FORMAT's ~R into
something that's compatible with the FileMaker example.  These
are only cosmetic corrections."
                 (string-capitalize
                  ;; replace "zero" with "no"
                  (regex-replace-all "zero"
                                     ;; remove "and"
                                     (regex-replace-all " and " string " ")
                                     "no"))))
          (if (plusp dollars)
            (string-append (normalize-text dollars-text) " and " (normalize-text cents-text))
            (normalize-text cents-text)))))))

;; formats a number according to the mask configured by the user
(define-plugin-function ("UserFormatNumber( textOrNumber )"
                         ;; use flags like in C++ example
                         :flags (logior +k-display-calc-fields+
                                        +k-display-auto-enter+
                                        +k-display-custom-functions+
                                        +k-display-generic+))
    ((number :string))
  (format-number-with-mask number *user-format*))

;; formats a number according to the mask provided as the first
;; argument
(define-plugin-function ("FormatNumber( formatString; textOrNumber )"
                         ;; use flags like in C++ example
                         :flags +k-display-calc-fields+)
    ((format :string) (number :string))
  (format-number-with-mask number format))

;;; And now for our own example functions:

;; returns a string with the calendar week and the corresponding year
;; separated by the string SEPARATOR - compare with FileMaker's function
;; WeekOfYearFiscal
(define-plugin-function "CalendarWeek( date {; separator} )"
    ((date :universal-time) &optional (separator :string "/"))
  (multiple-value-bind (week year)
      (calendar-week date)
    (format nil "~A~A~A" week separator year)))

;; returns a string with the make and model of the camera which took
;; the picture JPEG, i.e. JPEG should be a container holding a JPEG
;; taken with a digital camera
(define-plugin-function "CameraInfo( jpeg )"
    ((jpeg :binary-data))
  (destructuring-bind (&optional make model)
      (get-exif-infos jpeg :make :model)
    (format nil "~@[~A ~]~@[~A~]" make model)))

;; returns the timestamp of the time at which the picture JPEG was
;; shot, i.e. JPEG should be a container holding a JPEG taken with a
;; digital camera
(define-plugin-function ("PictureTakenAt( jpeg )" :result-type :timestamp)
    ((jpeg :binary-data))
  (let ((taken-at (get-exif-infos jpeg "DateTimeOriginal")))
    (make-date-time-object :universal-time taken-at)))