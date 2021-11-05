;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/specials.lisp,v 1.12 2015/08/23 14:48:42 edi Exp $

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

(defvar *default-weekday-names*
  '("Monday"
    "Tuesday"
    "Wednesday"
    "Thursday"
    "Friday"
    "Saturday"
    "Sunday")
  "Default values for the weekday names.  On Windows, locale-specific
values will be used instead if *USE-WIN32-LOCALE-INFO* is true.")

(defvar *default-month-names*
  '("January"
    "February"
    "March"
    "April"
    "May"
    "June"
    "July"
    "August"
    "September"
    "October"
    "November"
    "December")
  "Default values for the month names.  On Windows, locale-specific
values will be used instead if *USE-WIN32-LOCALE-INFO* is true.")

(defvar *default-first-day-of-week* 6
  "Default value for the day the week starts with.  0 is Monday, 1 is
Tuesday, and so on.  On Windows, locale-specific values will be used
instead if *USE-WIN32-LOCALE-INFO* is true.")

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The location of this source file.")

(defvar *up-arrow*
  (load-time-value
   (read-external-image (make-pathname :name "up"
                                       :type "bmp"
                                       :defaults *this-file*)
                        :transparent-color-index 1))
  "An image of a small arrow pointing upwards.  Used for buttons.")

(defvar *down-arrow*
  (load-time-value
   (read-external-image (make-pathname :name "down"
                                       :type "bmp"
                                       :defaults *this-file*)
                        :transparent-color-index 1))
  "An image of a small arrow pointing downwards.  Used for buttons.")

(defconstant +month-days+
  #(31 28 31 30 31 30 31 31 30 31 30 31)
  "An array which holds the length of each month \(in non-leap years)
in days.")

(defvar *window-color* :white
  "Default value for standard window background color.  On Windows,
the theme-specific value will be used instead if
*USE-WIN32-COLOR-INFO* is true.")

(defvar *window-text-color* :black
  "Default value for standard text color.  On Windows, the
theme-specific value will be used instead if *USE-WIN32-COLOR-INFO* is
true.")

(defvar *highlight-color* :midnightblue
  "Default value for background color of highlighted text.  On
Windows, the theme-specific value will be used instead if
*USE-WIN32-COLOR-INFO* is true.")

(defvar *highlight-text-color* :white
  "Default value for color of highlighted text.  On Windows, the
theme-specific value will be used instead if *USE-WIN32-COLOR-INFO* is
true.")

(defvar *inactive-caption-color* :grey40
  "Default value for background color of inactive caption text.  On
Windows, the theme-specific value will be used instead if
*USE-WIN32-COLOR-INFO* is true.")

(defvar *inactive-caption-text-color* :grey70
  "Default value for color of inactive caption text.  On Windows, the
theme-specific value will be used instead if *USE-WIN32-COLOR-INFO* is
true.")

(defvar *interface-geometry-cache* (make-hash-table :test 'eq)
  "Used internally to interface \(dialog) geometries.")

(defvar *collecting-pane-lock-counter* 0
  "Used internally to give each lock a different name.")

#+:win32
(defvar *use-win32-locale-info* t
  "Whether locale info provided by Windows should be used for weekday
names, month names, and the beginning of the week.")

#+:win32
(defvar *use-win32-color-info* t
  "Whether theme-specific info provided by Windows should be used for
text and background colors.")

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/midgets/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :midgets
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
