;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/completions.lisp,v 1.14 2015/03/06 12:54:25 edi Exp $

;;; Copyright (c) 2005-2015, Dr. Edmund Weitz.  All rights reserved.

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

;;; This code copied almost verbatim from SLIME, see
;;; <http://common-lisp.net/project/slime/>

(in-package :lw-add-ons)

(defun compound-prefix-match (prefix target)
  "Return true if PREFIX is a compound-prefix of TARGET.
Viewing each of PREFIX and TARGET as a series of substrings delimited
by hyphens, if each substring of PREFIX is a prefix of the
corresponding substring in TARGET then we call PREFIX a
compound-prefix of TARGET.

Examples:
\(compound-prefix-match \"foo\" \"foobar\") => t
\(compound-prefix-match \"m--b\" \"multiple-value-bind\") => t
\(compound-prefix-match \"m-v-c\" \"multiple-value-bind\") => NIL"
  (declare (type simple-string prefix target))
  (loop for ch across prefix
        with tpos = 0
        always (and (< tpos (length target))
                    (if (char= ch #\-)
                        (setf tpos (position #\- target :start tpos))
                        (char= ch (aref target tpos))))
        do (incf tpos)))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (search "::" string)))
    (values symbol package internp)))

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  (multiple-value-bind (name pos) 
      (if (zerop (length string))
          (values :|| 0)
          (let ((*package* +keyword-package+))
            (ignore-errors* (read-from-string string))))
    (if (and (or (keywordp name) (stringp name))
             (= (length string) pos))
        (find-package name))))

(defun guess-package-from-string (name &optional (default-package *package*))
  (or (and name
           (or (parse-package name)
               (find-package (string-upcase name))
               (parse-package (substitute #\- #\! name))))
      default-package))

(defun carefully-find-package (name default-package-name)
  "Find the package with name NAME, or DEFAULT-PACKAGE-NAME, or
the CL-USER package.  NAME and DEFAULT-PACKAGE-NAME can be nil."
  (let ((string (cond ((equal name "") "KEYWORD")
                      (t (or name default-package-name)))))
    (if string
        (guess-package-from-string string nil)
        +cl-user-package+)))

(defun parse-completion-arguments (string default-package-name)
  "Parse STRING as a symbol designator.
Return these values:
 SYMBOL-NAME
 PACKAGE-NAME, or nil if the designator does not include an explicit package.
 PACKAGE, the package to complete in
 INTERNAL-P, if the symbol is qualified with `::'."
  (multiple-value-bind (name package-name internal-p)
      (tokenize-symbol string)
    (let ((package (carefully-find-package package-name default-package-name)))
      (values name package-name package internal-p))))

(defun determine-case (string)
  "Return two booleans LOWER and UPPER indicating whether STRING
contains lower or upper case characters."
  (values (some #'lower-case-p string)
          (some #'upper-case-p string)))

(defun output-case-converter (input)
  "Return a function to case convert strings for output.
INPUT is used to guess the preferred case."
  (ecase (readtable-case *readtable*)
    (:upcase (if (some #'lower-case-p input) #'string-downcase #'identity))
    (:invert (lambda (output)
               (multiple-value-bind (lower upper) (determine-case output)
                 (cond ((and lower upper) output)
                       (lower (string-upcase output))
                       (upper (string-downcase output))
                       (t output)))))
    (:downcase (if (some #'upper-case-p input) #'string-upcase #'identity))
    (:preserve #'identity)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (unless package
    (setq package (symbol-package symbol)))
  (when package
    (multiple-value-bind (_ status)
        (find-symbol (symbol-name symbol) package)
      (declare (ignore _))
      (eq status :external))))

(defun find-matching-symbols (string package external test)
  "Return a list of symbols in PACKAGE matching STRING.
TEST is called with two strings.  If EXTERNAL is true, only external
symbols are returned."
  (let ((completions '())
        (converter (output-case-converter string)))
    (flet ((symbol-matches-p (symbol)
             (and (or (not external)
                      (symbol-external-p symbol package))
                  (funcall test string
                           (funcall converter (symbol-name symbol))))))
      (do-symbols (symbol package) 
        (when (symbol-matches-p symbol)
          (push symbol completions))))
    (delete-duplicates completions)))

(defun find-matching-packages (name matcher)
  "Return a list of package names matching NAME using the fuzzy
completion algorithm."
  (let ((to-match (string-upcase name)))
    (remove-if-not (lambda (x) (funcall matcher to-match x))
                   (mapcar (lambda (pkgname)
                             (concatenate 'string pkgname ":"))
                           (loop for package in (list-all-packages)
                                 collect (package-name package)
                                 append (package-nicknames package))))))

(defun format-completion-result (string internal-p package-name)
  (let ((prefix (cond (internal-p (format nil "~A::" package-name))
                      (package-name (format nil "~A:" package-name))
                      (t ""))))
    (values (concatenate 'string prefix string)
            (length prefix))))

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
 (mapcar (lambda (string)
            (format-completion-result string internal-p package-name))
          (sort strings #'string<)))

(defun completion-set (string default-package-name matchp)
  "Return the set of completion-candidates as strings."
  (multiple-value-bind (name package-name package internal-p)
      (parse-completion-arguments string default-package-name)
    (let* ((symbols (and package
                         (find-matching-symbols name
                                                package
                                                (and (not internal-p)
                                                     package-name)
                                                matchp)))
           (packs (and (not package-name)
                       (find-matching-packages name matchp)))
           (converter (output-case-converter name))
           (strings
            (mapcar converter
                    (nconc (mapcar #'symbol-name symbols) packs))))
      (format-completion-set strings internal-p package-name))))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun transpose-lists (lists)
  "Turn a list-of-lists on its side.
If the rows are of unequal length, truncate uniformly to the shortest.

For example:
\(transpose-lists '((ONE TWO THREE) (1 2)))
  => ((ONE 1) (TWO 2))"
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (transpose-lists (mapcar #'cdr lists))))))

(defun untokenize-completion (tokens)
  (format nil "~{~A~^-~}" tokens))

(defun tokenize-completion (string)
  "Return all substrings of STRING delimited by #\-."
  (loop with end
        for start = 0 then (1+ end)
        until (> start (length string))
        do (setq end (or (position #\- string :start start) (length string)))
        collect (subseq string start end)))

(defun longest-completion (completions)
  "Return the longest prefix for all COMPLETIONS.
COMPLETIONS is a list of strings."
  (untokenize-completion
   (mapcar #'longest-common-prefix
           (transpose-lists (mapcar #'tokenize-completion completions)))))

(defun completions (string default-package-name)
  "Return a list of completions for a symbol designator STRING.  

The result is the list \(COMPLETION-SET
COMPLETED-PREFIX). COMPLETION-SET is the list of all matching
completions, and COMPLETED-PREFIX is the best (partial)
completion of the input string.

If STRING is package qualified the result list will also be
qualified.  If string is non-qualified the result strings are
also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

The way symbols are matched depends on the symbol designator's
format. The cases are as follows:
  FOO      - Symbols with matching prefix and accessible in the buffer package.
  PKG:FOO  - Symbols with matching prefix and external in package PKG.
  PKG::FOO - Symbols with matching prefix and accessible in package PKG."
  (let ((completion-set (completion-set string default-package-name 
                                        *completion-match-function*)))
    (values completion-set (longest-completion completion-set))))
