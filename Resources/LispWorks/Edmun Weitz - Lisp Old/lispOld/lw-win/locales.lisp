;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-WIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-win/locales.lisp,v 1.6 2010/08/10 16:55:42 edi Exp $

;;; Copyright (c) 2008-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :lw-win)

(defconstant +sdaynames+
  `(,+locale-sdayname1+
    ,+locale-sdayname2+
    ,+locale-sdayname3+
    ,+locale-sdayname4+
    ,+locale-sdayname5+
    ,+locale-sdayname6+
    ,+locale-sdayname7+)
  "List of all Win32 constants for long weekday names.")

(defconstant +smonthnames+
  `(,+locale-smonthname1+
    ,+locale-smonthname2+
    ,+locale-smonthname3+
    ,+locale-smonthname4+
    ,+locale-smonthname5+
    ,+locale-smonthname6+
    ,+locale-smonthname7+
    ,+locale-smonthname8+
    ,+locale-smonthname9+
    ,+locale-smonthname10+
    ,+locale-smonthname11+
    ,+locale-smonthname12+)
  "List of all Win32 constants for long month names.")

(define-foreign-function (%get-locale-info "GetLocaleInfo" :dbcs)
    ((locale :unsigned-long)
     (lc-type :unsigned-long)
     (lc-data :pointer)
     (size :int))
  :lambda-list (lc-type &optional (lc-data *null-pointer*) (size 0) (locale +locale-user-default+))
  :result-type :int
  :documentation "Win32 function to get locale-specific information.")

(defun get-locale-info (lc-type)
  "Ask Windows for locale-specific information specified by the
integer identifier LC-TYPE and returns the answer as a \(Lisp)
string."
  ;; first call with null pointer and length zero to get length of result
  (let ((length (with-error-check () (%get-locale-info lc-type))))
    ;; now allocate foreign string of the right length and call again
    (with-dynamic-foreign-objects ()
      (let ((lc-data (allocate-dynamic-foreign-object :type (win32-char-type)
                                                      :nelems length)))
        (with-error-check () (%get-locale-info lc-type lc-data length))
        (convert-from-foreign-string lc-data
                                     :external-format (win32-external-format))))))

(defun get-weekday-names ()
  "Returns a list of all seven \(long) weekday names \(beginning with
the name for Monday) as specified by the current Windows locale."
  (loop for day-name in +sdaynames+
        collect (get-locale-info day-name)))

(defun get-month-names ()
  "Returns a list of all twelve \(long) month names \(beginning with
the name for January) as specified by the current Windows locale."
  (loop for month-name in +smonthnames+
        collect (get-locale-info month-name)))

(defun get-first-day-of-week ()
  "Returns the first day of the week as specified by the current
Windows locale as an integer - 0 is Monday, 1 is Tuesday, and so on."
  (parse-integer (get-locale-info +locale-ifirstdayofweek+)))

