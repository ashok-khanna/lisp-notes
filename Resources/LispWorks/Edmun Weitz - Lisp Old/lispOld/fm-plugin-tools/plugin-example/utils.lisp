;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PLUGIN-EXAMPLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/plugin-example/utils.lisp,v 1.9 2010/07/22 09:38:09 edi Exp $

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

(defun read-config-values ()
  "Reads configuration values which should persist between
invocations of the plug-in from the Windows registry and sets the
corresponding Lisp variables."
  (multiple-value-bind (user-format foundp)
      (plugin-preference "Configuration" "UserFormat")
    (when foundp
      (setq *user-format* user-format))))

(defun store-config-values ()
  "Writes configuration values which should persist between
invocations of the plug-in into the Windows registry."
  (setf (plugin-preference "Configuration" "UserFormat")
        *user-format*))

(defun get-decimal-separator ()
  "Returns the decimal separator as specified by the operating
system's locale information as a character."
  (let ((decimal-separator #+:win32 (get-locale-info +locale-sdecimal+)
                           #+:macosx (get-locale-value-as-string +k-cf-locale-decimal-separator+)))
    (case (length decimal-separator)
      ;; we ignore the case that there might be more than one
      ;; character
      (1 (char decimal-separator 0))
      ;; we return NIL if the string is empty
      (otherwise nil))))

(defun init-plugin ()
  "Called when the plug-in is initialized.  Reads configuration
values and gets decimal separator character."
  (read-config-values)
  ;; use #\. as default
  (setq *decimal-separator* (or (get-decimal-separator) #\.)))

(defun filter-number (string &optional keep-decimal-separator-p)
  "Returns STRING with all characters removed that aren't decimal
digits.  Removes the decimal separator unless
KEEP-DECIMAL-SEPARATOR-P is true.  Leading zeros are also removed
if they're insignificant \(i.e. in front of the separator)."
  (with-output-to-string (out nil :element-type 'lw:simple-char)
    (loop with decimal-separator-seen-p
          with non-zero-seen-p
          for char across string
          for decimal-separator-p = (and (eql char *decimal-separator*)
                                     keep-decimal-separator-p
                                     (setq non-zero-seen-p t)
                                     (not decimal-separator-seen-p)
                                     (setq decimal-separator-seen-p t))
          when (cond (decimal-separator-p)
                     ((char= char #\0) non-zero-seen-p)
                     ((char<= #\1 char #\9) (setq non-zero-seen-p t)))
          do (write-char char out))))

(defun ensure-two-decimal-places (string)
  "STRING is assumed to consist of only digits and at most one
decimal separator.  The result is a STRING with exactly two
digits behind the separator.  A separator is added if necessary.
Likewise, trailing zeros are added if needed.  If there are
already more than two trailing digits, the spare ones are removed
without rounding."
  (let* ((length (length string))
         (point-pos (or (position *decimal-separator* string) length)))
    (when (< (+ 3 point-pos) length)
      ;; remove excess trailing digits
      (setq length (+ 3 point-pos)
            string (subseq string 0 length)))
    ;; maybe add zeros and separator
    (string-append string
                   (case (- length point-pos)
                     (0 (format nil "~A00" *decimal-separator*))
                     (1 "00")
                     (2 "0")
                     (otherwise "")))))

(defun format-number-with-mask (number mask)
  "NUMBER is assumed to be a string \(!) which is converted with
FILTER-NUMBER to one consisting solely of digits \(without a
decimal separator).  The result of this function is the string
MASK with each occurrence of #\# replaced with one of the digits
of NUMBER.  The mask is processed from right to left.  If there
aren't enough digits, zeros are added.  If there are excess
digits, an error is signaled."
  (let ((filtered-number (filter-number number)))
    (when (zerop (length filtered-number))
      (error "We don't accept strings that don't represent non-zero numbers."))
    (nreverse
     (with-output-to-string (out nil :element-type 'lw:simple-char)
       (loop with digits = (nreverse (coerce filtered-number 'list))
             for char across (reverse mask)
             do (write-char (case char
                              (#.#\# (or (pop digits) #\0))
                              (otherwise char))
                            out)
             finally (when digits
                       (error "There are unused digits.")))))))

(defun date-diff (time other-year)
  "Returns which calendar week of OTHER-YEAR the universal time TIME
is in.  The result will be negative if OTHER-YEAR is too far in the
future \(and too high if OTHER-YEAR is too far in the past)."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (declare (ignore second minute hour))
    ;; normalize both timestamps to 12 o'clock
    (let* ((noon-time (encode-universal-time 0 0 12 date month year))
           (jan-4th (encode-universal-time 0 0 12 4 1 other-year))
           (day-of-week (nth-value 6 (decode-universal-time jan-4th))))
      (floor (+ 1.05 ; add one because difference below is 0-based,
                     ; add .05 to account for rounding errors
                ;; difference in weeks between TIME and Jan 4
                (/ (- noon-time jan-4th) +seconds-per-week+)
                ;; we want the difference between TIME and the Monday
                ;; of the Jan 4 week, so we have to add some fractions
                ;; of a week if Jan 4 wasn't a Monday
                (/ day-of-week 7))))))
    
(defun calendar-week (time)
  "Returns two values, the calendar week \(according to ISO 8601) of
the universal time TIME and the corresponding year.  Note that the
year can be different from TIME's year."
  ;; see http://www.cl.cam.ac.uk/~mgk25/iso-time.html
  ;; for information about ISO 8601
  (loop with year = (nth-value 5 (decode-universal-time time))
        for test-year downfrom (1+ year)
        for result = (date-diff time test-year)
        until (plusp result)
        finally (return (values result test-year))))

(defun get-exif-infos (binary-data &rest tag-designators)
  "It is assumed that BINARY-DATA is a BINARY-DATA-OBJECT which has a
JPEG stream.  This function tries to return the Exif info designated
by TAG-DESIGNATORS which can be found in the corresponding image.  The
result is a list of Lisp objects, or one Lisp object if there is only
one tag designator."
  ;; find the stream, stop if none can be found, otherwise get all the
  ;; data from this stream as a vector of octets
  (let* ((jpg-index (or (get-index binary-data "JPEG")
                        (return-from get-exif-infos)))
         (jpg-data (get-data binary-data jpg-index)))
    ;; convert the vector into a stream
    (with-input-from-sequence (jpg-stream jpg-data)
      (let* ((exif (ignore-errors (make-exif jpg-stream)))
             (result (loop for tag-designator in tag-designators
                           collect (and exif
                                        (ignore-errors 
                                          (parsed-exif-value tag-designator exif))))))
        (cond ((cdr result) result)
              (t (first result)))))))
    