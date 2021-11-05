;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/util.lisp,v 1.18 2013/08/23 08:04:12 edi Exp $

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

(defun sans (plist &rest keys)
  "Returns PLIST with keyword arguments from KEYS removed."
  ;; stolen from Usenet posting <3247672165664225@naggum.no> by Erik
  ;; Naggum
  (let ((sans ()))
    (loop
      (let ((tail (nth-value 2 (get-properties plist keys))))
        ;; this is how it ends
        (unless tail
          (return (nreconc sans plist)))
        ;; copy all the unmatched keys
        (loop until (eq plist tail) do
              (push (pop plist) sans)
              (push (pop plist) sans))
        ;; skip the matched key
        (setq plist (cddr plist))))))

(defun split-plist (plist &rest keys)
  "Returns as two values two property lists where the first one
contains the key/value pairs from PLIST corresponding to the keys in
KEYS and the second one contains the remaining pairs from PLIST."
  (loop for (key value . nil) on plist by #'cddr
        when (find key keys :test #'eq)
        collect key into found
        and collect value into found
        else collect key into not-found
        and collect value into not-found
        finally (return (values found not-found))))

#+:win32
(defun get-first-day-of-week ()
  "Returns the first day of the week as an integer where 0 is Monday,
and so on.  Result depends on *DEFAULT-FIRST-DAY-OF-WEEK* or on
locale-specific information if *USE-WIN32-LOCALE-INFO* is true."
  (cond (*use-win32-locale-info*
         (lw-win:get-first-day-of-week))
        (t *default-first-day-of-week*)))

#-:win32
(defun get-first-day-of-week ()
  "Returns the first day of the week as an integer where 0 is Monday,
and so on.  Result depends on *DEFAULT-FIRST-DAY-OF-WEEK*."
  *default-first-day-of-week*)

#+:win32
(defun %get-weekday-names ()
  "Returns the weekday names beginning with Monday as a list of
strings.  Result depends on *DEFAULT-WEEKDAY-NAMES* or on
locale-specific information if *USE-WIN32-LOCALE-INFO* is true."
  (cond (*use-win32-locale-info*
         (lw-win:get-weekday-names))
        (t *default-weekday-names*)))

#-:win32
(defun %get-weekday-names ()
  "Returns the weekday names beginning with Monday as a list of
strings.  Result depends on *DEFAULT-WEEKDAY-NAMES*."
  *default-weekday-names*)

(defun get-weekday-names (&optional (first-day-of-week (get-first-day-of-week)))
  "Returns the weekday names as a list of strings.  Result depends on
*DEFAULT-WEEKDAY-NAMES* or \(on Windows) on locale-specific
information if *USE-WIN32-LOCALE-INFO* is true.  The list begins with
the first day of the week as specified by the integer
FIRST-DAY-OF-WEEK."
  (let ((raw-names (%get-weekday-names)))
    (append (nthcdr first-day-of-week raw-names)
            (butlast raw-names (- 7 first-day-of-week)))))

(defun get-weekday-chars (&optional (first-day-of-week (get-first-day-of-week)))
  "Returns the first characters of the weekday names as a list.
Result depends on *DEFAULT-WEEKDAY-NAMES* or \(on Windows) on
locale-specific information if *USE-WIN32-LOCALE-INFO* is true.  The
list begins with the first day of the week as specified by the integer
FIRST-DAY-OF-WEEK."
  (mapcar (lambda (name)
            (char name 0))
          (get-weekday-names first-day-of-week)))

#+:win32
(defun get-month-names ()
  "Returns the month names beginning with January as a list of
strings.  Result depends on *DEFAULT-MONTH-NAMES* or on
locale-specific information if *USE-WIN32-LOCALE-INFO* is true."
  (cond (*use-win32-locale-info*
         (lw-win:get-month-names))
        (t *default-month-names*)))

#-:win32
(defun get-month-names ()
  "Returns the month names beginning with January as a list of
strings.  Result depends on *DEFAULT-MONTH-NAMES*."
  *default-month-names*)

(defun current-second ()
  "Returns the second of the current universal time."
  (decode-universal-time (get-universal-time)))

(defun current-minute ()
  "Returns the minute of the current universal time."
  (nth-value 1 (decode-universal-time (get-universal-time))))

(defun current-hour ()
  "Returns the hour of the current universal time."
  (nth-value 2 (decode-universal-time (get-universal-time))))

(defun current-day ()
  "Returns the day of the current universal time."
  (nth-value 3 (decode-universal-time (get-universal-time))))

(defun current-month ()
  "Returns the month of the current universal time."
  (nth-value 4 (decode-universal-time (get-universal-time))))

(defun current-year ()
  "Returns the year of the current universal time."
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun number-of-days (month year)
  "Returns the number of days of the month MONTH in the year YEAR
where MONTH is an integer starting with one for January.  Handles leap
years correctly."
  (case month
    (2 (if (system::leap-year-p year) 29 28))
    (otherwise (aref +month-days+ (1- month)))))

#+:win32
(defun get-sys-color (index)
  "Returns a CAPI RGB color specification corresponding to the integer
identifier INDEX if the color is supported and if
*USE-WIN32-COLOR-INFO* is true.  Returns NIL otherwise.."
  (and *use-win32-color-info*
       (lw-win:get-sys-color index)))

(defun get-window-color ()
  "Returns the standard window background color.  Result depends on
*WINDOW-COLOR* or \(on Windows) on theme-specific information if
*USE-WIN32-COLOR-INFO* is true."
  (or #+:win32 (get-sys-color lw-win:+color-window+)
      *window-color*))

(defun get-window-text-color ()
  "Returns the standard text color.  Result depends on
*WINDOW-TEXT-COLOR* or \(on Windows) on theme-specific information if
*USE-WIN32-COLOR-INFO* is true."
  (or #+:win32 (get-sys-color lw-win:+color-windowtext+)
      *window-text-color*))

(defun get-highlight-color ()
  "Returns the background color for highlighted text.  Result depends
on *HIGHLIGHT-COLOR* or \(on Windows) on theme-specific information if
*USE-WIN32-COLOR-INFO* is true."
  (or #+:win32 (get-sys-color lw-win:+color-highlight+)
      *highlight-color*))

(defun get-highlight-text-color ()
  "Returns the color for highlighted text.  Result depends on
*HIGHLIGHT-TEXT-COLOR* or \(on Windows) on theme-specific information
if *USE-WIN32-COLOR-INFO* is true."
  (or #+:win32 (get-sys-color lw-win:+color-highlighttext+)
      *highlight-text-color*))

(defun get-inactive-caption-color ()
  "Returns the background color for inactive caption text.  Result
depends on *INACTIVE-CAPTION-COLOR* or \(on Windows) on theme-specific
information if *USE-WIN32-COLOR-INFO* is true."
  (or #+:win32 (get-sys-color lw-win:+color-inactivecaption+)
      *inactive-caption-color*))

(defun get-inactive-caption-text-color ()
  "Returns the text color for inactive caption text.  Result depends
on *INACTIVE-CAPTION-TEXT-COLOR* or \(on Windows) on theme-specific
information if *USE-WIN32-COLOR-INFO* is true."
  (or #+:win32 (get-sys-color lw-win:+color-inactivecaptiontext+)
      *inactive-caption-text-color*))

(defun split (char string)
  "Cuts the string STRING into pieces at every occurrence of the
character CHAR and returns the results as a list of strings."
  (loop for previous-pos = 0 then (1+ pos)
        for pos = (position char string
                            :start previous-pos
                            :test #'char=)
        while pos
        collect (subseq string previous-pos pos) into result
        finally do (return (nconc result
                                  (list (subseq string previous-pos (length string)))))))

(defun safe-parse-integer (string)
  "If the string STRING represents an integer number, the
corresponding integer is returned.  If STRING consists only of spaces,
the result is zero.  Otherwise NIL is returned."
  (cond ((every (lambda (char)
                  (char= char #\Space))
                string)
         0)
        (t (ignore-errors (parse-integer string)))))

(defun clean-number (string bound old-string)
  "If STRING represents an integer that is smaller than BOUND, STRING
is returned \(but spaces on the right and then on the left are removed
until STRING is at most two characters long), otherwise OLD-STRING is
returned."
  (let ((number (safe-parse-integer string)))
    (cond ((and number
                (< number bound))
           (when (> (length string) 2)
             ;; remove all spaces on the right
             (setq string (string-right-trim " " string)))
           ;; remove spaces on the left one at a time
           (loop while (> (length string) 2)
                 do (setq string (subseq 1 (length string))))
           string)
          (t old-string))))

(defun colon-positions (string)
  "STRING must be a string containing at least one colon.  Returns the
positions of the first two colons within STRING as two values."
  (let ((pos1 (position #\: string :test #'char=)))
    (values pos1
            (position #\: string
                      :test #'char=
                      :start (1+ pos1)))))

(defun find-interval (target pos1 pos2 end &optional which)
  "POS1, POS2, and END must be non-negative integers with \(< POS1
POS2 END), and TARGET a non-negative INTEGER below END.  Returns as
its first value the one interval amongst [0 POS1), [POS1 POS2), and
\[POS2 END) \(as a two-element list) which contains TARGET.  The second
value is an integer between 0 and 2 denoting which of these three
intervals was returned.  If WHICH is not NIL, it should be one of 0,
1, or 2 and it'll determine the interval to be returned, i.e. TARGET
will be ignored in this case."
  (values (list
           ;; go from right to left and find greatest upper bound
           (loop for i in (list (1+ pos2) (1+ pos1) 0)
                 for j from 2 downto 0
                 when (or (eql which j)
                          (and (null which)
                               (<= i target)))
                 do (setq which j) and
                 return i)
           ;; now find corresponding lower bound
           (loop for i in (list pos1 pos2 end)
                 for j from 0 to 2
                 when (eql which j)
                 return i))
          which))

(defun random-color ()
  "Creates and returns a random RGB color specification."
  (color:make-rgb (random 1.0) (random 1.0) (random 1.0)))