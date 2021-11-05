;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-DOC; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-doc/parse.lisp,v 1.14 2015/06/08 19:01:22 edi Exp $

;;; Copyright (c) 2005-2015, Dr. Edmund Weitz. All rights reserved.

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

(in-package :lw-doc)

(defun save-links (shortcut symbol type links)
  "Puts all links which can be found in the string LINKS into the
hash table entry corresponding to SYMBOL and TYPE."
  (setq symbol (remove-html-entities symbol))
  (do-register-groups (link (#'parse-integer page-number))
      ("(?is)<EM\\s+CLASS=\"(?:MyCharTag|IndexPageNum)\">\\s*<A\\s+HREF=\"(.*?)\"\\s+CLASS=\"Index\">\\s*(\\d+)\\s*</A></EM>" links)
    (pushnew (list shortcut page-number link)
             (gethash (list symbol type) *link-table*)
             :test #'first-two-equal)))

(defun parse-one-file (shortcut file)
  "Parses the \(LW documentation index) file FILE for symbols and
editor commands and hands them over to SAVE-LINKS."
  (let ((contents (file-contents file)))
    (do-register-groups (symbol links)
        ("(?is)<CODE\\s+CLASS=\"Code\">\\s*([^<]*?)\\s*</CODE>[^<]*(<EM\\s*CLASS=\"(?:MyCharTag|IndexPageNum)\".*?)\\s*</P>" contents)
      (save-links shortcut symbol 'code links))
    (do-register-groups (symbol links)
        ("(?is)<B\\s+CLASS=\"Bold\">\\s*([^<]*?)\\s*</B>[^<]*(<EM\\s*CLASS=\"(?:MyCharTag|IndexPageNum)\".*?)\\s*</P>" contents)
      (save-links shortcut symbol 'bold links))))

(defun parse-files ()
  "Parses all files listed in *INDEX-PAGES* with PARSE-ONE-FILE."
  (clrhash *link-table*)
  (loop for (shortcut file%) in *index-pages*
        when file% do
        (parse-one-file shortcut (merge-pathnames file% *docs-base-path*))))

(defun find-boundaries (symbol)
  "Returns a list of indexes into the string SYMBOL which are
used to `permute' the string."
  (let ((result (list 0)))
    (do-matches (start end "(?i)(?<=\\W)\\b" symbol)
      (declare (ignore end))
      (push start result))
    (nreverse result)))

(defun fill-sorted-table ()
  "Fills the hash table *SORTED-TABLE* with all entries found in
*LINK-TABLE*."
  (loop for (symbol type) being the hash-keys of *link-table*
        using (hash-value link-info) do
          (loop for index in (find-boundaries symbol)
                for char = (normalize-char (char symbol index)) do
                  (push (list symbol index type link-info)
                        (gethash char *sorted-table*)))))

(defun sort-sorted-table-rows ()
  "Sorts the hash values of *SORTED-TABLE* in alphabetical order
\(starting at the position denoted by the `permutation' index)."
  (loop for char being the hash-keys of *sorted-table* do
    (setf (gethash char *sorted-table*)
            (stable-sort (sort (gethash char *sorted-table*)
                               #'string-lessp
                               :key #'first)
                         #'string-lessp
                         :key (lambda (link-info)
                                (destructuring-bind (symbol index &rest rest)
                                    link-info
                                  (declare (ignore rest))
                                  (nsubseq symbol index)))))))
