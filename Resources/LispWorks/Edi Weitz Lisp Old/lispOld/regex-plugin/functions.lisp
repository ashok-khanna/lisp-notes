;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGEX-PLUGIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/regex-plugin/functions.lisp,v 1.23 2008/01/07 07:36:02 edi Exp $

;;; Copyright (c) 2006-2008, Dr. Jens Teich and Dr. Edmund Weitz.  All rights reserved.

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

(in-package :regex-plugin)

(define-plugin-function "Version"
    ()
  ;; just return the version string
  (version-string))

;; this function just scans a string and returns a boolean result
;; this is obviously very easy - just four lines
(define-plugin-function "Scan ( regex ; target {; flags} )"
    ((regex :string) (target :string) &optional (flags :string))
  (with-flags (flags)
    (boolean-value (ppcre:scan (get-cached-regex regex) target))))

;; for the rest of the functions see the documentation for RegexPlugIn
(define-plugin-function "GetText ( regex ; target {; flags; register} )"
    ((regex :string) (target :text) &optional (flags :string) (register :integer))
  (with-flags (flags)
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan (get-cached-regex regex) (as-string target))
      (flet ((subseq% (from to)
               "Returns a TEXT-OBJECT representing the substring of
TARGET from position FROM to position TO."
               (let ((sub-part (make-text-object)))
                 (set-text sub-part target :position from :size (- to from))
                 sub-part)))
        (cond ((null match-start) 0)
              ((or (null register)
                   (zerop register))
               (subseq% match-start match-end))
              ;; registers are traditionally counted started with 1,
              ;; so we have to subtract 1 here and below
              ((aref reg-starts (1- register))
               (subseq% (aref reg-starts (1- register)) (aref reg-ends (1- register))))
              (t 0))))))

(define-plugin-function "MatchStart ( regex ; target {; flags; register} )"
    ((regex :string) (target :string) &optional (flags :string) (register :integer))
  (with-flags (flags)
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan (get-cached-regex regex) target)
      (declare (ignore match-end reg-ends))
      (cond ((null match-start) -1) ;; -1 means 'False' here
            ((or (null register)
                 (zerop register)) match-start)
            (t (or (aref reg-starts (1- register)) -1))))))

(define-plugin-function "MatchEnd ( regex ; target {; flags; register} )"
    ((regex :string) (target :string) &optional (flags :string) (register :integer))
  (with-flags (flags)
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan (get-cached-regex regex) target)
      (declare (ignore reg-starts))
      (cond ((null match-start) -1)
            ((or (null register)
                 (zerop register)) match-end)
            (t (or (aref reg-ends (1- register)) -1))))))

(define-plugin-function "Positions ( regex ; target {; flags} )"
    ((regex :string) (target :string) &optional (flags :string))
  (with-flags (flags)
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan (get-cached-regex regex) target)
      (cond ((null match-start) 0)
            (t (format nil "~A|~A~{|~A|~A~}"
                       match-start match-end
                       (loop for i below (length reg-starts)
                             collect (or (aref reg-starts i) "-")
                             collect (or (aref reg-ends i) "-"))))))))
      
(defun build-replacement-template (replacement-text)
  "Converts a replacement TEXT-OBJECT for REGEX-REPLACE-ALL into
a replacement template which is an S-expression."
  ;; this function is equivalent to the (internal) CL-PPCRE function
  ;; of the same name (see file api.lisp there) but it works with
  ;; TEXT-OBJECT objects instead of strings
  (flet ((subseq% (from &optional (to (size replacement-text)))
           "Returns a TEXT-OBJECT representing the substring of
REPLACEMENT-TEXT from position FROM to position TO."
           (let ((sub-part (make-text-object)))
             (set-text sub-part replacement-text :position from :size (- to from))
             sub-part)))
    (let ((from 0)
          (collector '())
          (replacement-string (as-string replacement-text)))
      ;; look for character sequences with special meaning (e.g. "\\1")
      (ppcre:do-matches (match-start match-end "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')"
                                     replacement-string)
        (when (< from match-start)
          (push (subseq% from match-start) collector))
        (let* ((parse-start (position-if #'digit-char-p
                                         replacement-string
                                         :start match-start
                                         :end match-end))
               (token (if parse-start
                        (1- (parse-integer replacement-string
                                           :start parse-start
                                           :junk-allowed t))
                        (case (char replacement-string (1+ match-start))
                          ((#\&) :match)
                          ((#\`) :before-match)
                          ((#\') :after-match)
                          ((#\\) :backslash)))))
          (when (and (numberp token) (< token 0))
            (error "Illegal substring ~S in replacement string"
                   (subseq replacement-string match-start match-end)))
          ;; collect the token and the corresponding character style
          (push (list token (get-style replacement-text (1+ match-start)))
                collector))
        (setq from match-end))
      (when (< from (length replacement-string))
        (push (subseq% from) collector))
      (nreverse collector))))
        
(defun build-replacement (replacement-template
                          target-text
                          match-start match-end
                          reg-starts reg-ends)
  "Accepts a replacement template and the current values from the
matching process in REGEX-REPLACE-ALL and returns the
corresponding text object."
  ;; this function is equivalent to the (internal) CL-PPCRE function
  ;; of the same name (see file api.lisp there) but it works with
  ;; TEXT-OBJECT objects instead of strings
  (let ((reg-bound (if reg-starts
                     (array-dimension reg-starts 0)
                     0))
        (result (make-text-object))
        (end (size target-text)))
    (flet ((append% (what &key (start 0) (end (size what)) style)
             "Appends the substring of the text object WHAT from
position START to position END to the text object RESULT.  If
STYLE is not NIL, it is a character style which is applied to the
appended text object."
             (let ((size (- end start))
                   (current-end (size result)))
               (append-text result what :position start :size size)
               (when style
                 (set-style result style current-end size)))))
      (dolist (thing replacement-template)
        (typecase thing
          (text-object
           (append% thing))
          (cons
           (let ((token (first thing))
                 (style (second thing)))
             (typecase token
               (integer
                (when (>= token reg-bound)
                  (error "Reference to non-existent register ~A in replacement string" (1+ token)))
                (when (svref reg-starts token)
                  (append% target-text
                           :start (svref reg-starts token)
                           :end (svref reg-ends token)
                           :style (and *apply-style* style))))
               (symbol
                (case token
                  ((:backslash)
                   (append% (make-text-object "\\") :style style))
                  ((:match)
                   (append% target-text 
                            :start match-start
                            :end match-end
                            :style (and *apply-style* style)))
                  ((:before-match)
                   (append% target-text
                            :start 0
                            :end match-start
                            :style (and *apply-style* style)))
                  ((:after-match)
                   (append% target-text
                            :start match-end
                            :end end
                            :style (and *apply-style* style)))))))))))
    result))

(defun replace-aux (target-text replacement-text pos-list reg-list)
  "Auxiliary function used by REGEX-REPLACE-ALL.  POS-LIST
contains a list with the start and end positions of all matches
while REG-LIST contains a list of arrays representing the
corresponding register start and end positions."
  ;; this function is equivalent to the (internal) CL-PPCRE function
  ;; of the same name (see file api.lisp there) but it works with
  ;; TEXT-OBJECT objects instead of strings
  (let* ((replacement-template (build-replacement-template replacement-text))
         (result (make-text-object)))
    (flet ((append% (what &key (start 0) (end (size what)))
             (append-text result what :position start :size (- end start))))
      (loop for (from to) on (append (list 0) pos-list (list (size target-text)))
            for replace = nil then (and (not replace) to)
            for reg-starts = (if replace (pop reg-list) nil)
            for reg-ends = (if replace (pop reg-list) nil)
            for curr-replacement = (if replace
                                     (build-replacement replacement-template
                                                        target-text
                                                        from to
                                                        reg-starts reg-ends)
                                     nil)
            while to
            if replace do (append% curr-replacement)
            else do (append% target-text :start from :end to)))
    result))

(defun regex-replace-all (regex target-text replacement-text)
  "Like CL-PPCRE:REGEX-REPLACE-ALL but for text objects instead
of strings.  Note that REGEX is still a string."
  ;; this function is equivalent to the CL-PPCRE function of the same
  ;; name (see file api.lisp there) but it works with TEXT-OBJECT
  ;; objects instead of strings
  (let ((pos-list '())
        (reg-list '()))
    (ppcre:do-scans (match-start match-end reg-starts reg-ends regex (as-string target-text))
      (push match-start pos-list)
      (push match-end pos-list)
      (push reg-starts reg-list)
      (push reg-ends reg-list))
    (if pos-list
      (replace-aux target-text replacement-text
                   (nreverse pos-list)
                   (nreverse reg-list))
      target-text)))

(defun regex-replace (regex target-text replacement-text)
  "Like CL-PPCRE:REGEX-REPLACE but for text objects instead of
strings.  Note that REGEX is still a string."
  ;; this function is equivalent to the CL-PPCRE function of the same
  ;; name (see file api.lisp there) but it works with TEXT-OBJECT
  ;; objects instead of strings
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (ppcre:scan regex (as-string target-text))
    (if match-start
      (replace-aux target-text replacement-text
                   (list match-start match-end)
                   (list reg-starts reg-ends))
      target-text)))

(defun apply-evaluation (text)
  "Goes through the text object TEXT looking for occurrences of
${...} and replaces those with the results of evaluating the text
enclosed in braces in FileMaker."
  (let (to-do-list)
    ;; loop through all matches
    (ppcre:do-matches (start end "\\${.*?}" (as-string text))
      (let ((text-part (make-text-object)))
        ;; get corresponding substring of TEXT
        (set-text text-part text :position (+ start 2) :size (- end start 3))
        ;; evaluate
        (let ((result (evaluate text-part)))
          (when result
            ;; get result as text (object)
            (let ((evaluated-text-part (as-text-object result)))
              (when *apply-style*
                ;; maybe apply character style
                (set-style evaluated-text-part
                           (get-style text (+ start 2))
                           0 (size evaluated-text-part)))
              ;; collect resulting text object and positions within
              ;; TEXT
              (push (list evaluated-text-part start end) to-do-list))))))
    ;; now perform the replacement; note that we work in reverse order
    ;; which we need to do in order to get the positions right
    (dolist (to-do to-do-list)
      (destructuring-bind (text-part start end)
          to-do
        ;; first delete ${...} part
        (delete-text text start :size (- end start))
        ;; then insert evaluation result
        (insert-text text text-part :position start)))))

(define-plugin-function "Replace ( regex ; target ; replacement {; flags} )"
    ((regex :string) (target-text :text) (replacement-text :text) &optional (flags :string))
  (with-flags (flags)
    (let ((result-text
           (regex-replace-all (get-cached-regex regex) target-text replacement-text)))
      (when *evaluatep*
        (apply-evaluation result-text))
      result-text)))

(define-plugin-function "ReplaceOne ( regex ; target ; replacement {; flags} )"
    ((regex :string) (target-text :text) (replacement-text :text) &optional (flags :string))
  (with-flags (flags)
    (let ((result-text
           (regex-replace (get-cached-regex regex) target-text replacement-text)))
      (when *evaluatep*
        (apply-evaluation result-text))
      result-text)))
