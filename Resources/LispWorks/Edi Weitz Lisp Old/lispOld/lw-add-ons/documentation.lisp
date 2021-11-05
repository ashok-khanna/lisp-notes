;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/documentation.lisp,v 1.20 2015/05/29 18:23:24 edi Exp $

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

(in-package :lw-add-ons)

(defun manual-dir (&optional relative-path)
  "Returns a namestring for the LW browsable documentation
directory, optionally appending the string RELATIVE-PATH."
  (namestring (sys:lispworks-dir
               (format nil
                       #+(or :lispworks6.1 :lispworks7)
                       "manual/online/~A"
                       #-(or :lispworks6.1 :lispworks7)
                       "manual/online/web/~A"
                       (or relative-path "")))))

(defun remove-backslashes (string)
  "Returns STRING with backslashes replaced with slashes."
  (regex-replace-all "\\\\" string "/"))

(defun make-file-url (pathspec)
  "Accepts a pathname designator and returns a corresponding file
URL."
  (format nil "file:///~A"
          (remove-backslashes (namestring pathspec))))

(defun doc-entry (entry)
  "Returns the URL for the entry ENTRY."
  (or (ignore-errors* (do-hyperdoc-lookup entry))
      (gethash entry *doc-hash*)))

(defun add-doc-entry (entry link)
  "Sets the URL for the entry ENTRY to be LINK."
  (setf (gethash entry *doc-hash*) link))

(defun add-clhs-entry (entry link)
  "Accepts a CLHS entry and the name of the corresponding HTML
file \(maybe with fragment part) and creates the right *DOC-HASH*
entry."
  (let ((clhs-prefix
          (load-time-value
            (make-file-url (manual-dir "CLHS/Body/")))))
    (add-doc-entry entry (format nil "~A~A" clhs-prefix link))))

(defun collect-lw-links ()
  "Puts entries for all LW-specific functions into *DOC-HASH*
using functionality from the LW-DOC module."
  (lw-doc:parse-files)
  (let ((lw-doc:*link-prefix*
          (make-file-url (manual-dir))))
    (loop for (symbol nil) being the hash-keys of lw-doc::*link-table*
          using (hash-value ((shortcut nil link) . nil))
          do (add-doc-entry symbol (lw-doc::make-link shortcut link)))))

(defun collect-clhs-links ()
  "Puts all CLHS `standard' entries into *DOC-HASH* using the
`Map_Sym.txt' file."
  (with-open-file (map (merge-pathnames "Map_Sym.txt"
                                        (manual-dir "CLHS/Data/")))
    (loop for symbol-line = (read-line map nil nil)
          for link-line = (read-line map nil nil)
          while (and symbol-line link-line)
          do (add-clhs-entry symbol-line (subseq link-line 8)))))

(defun collect-clhs-add-on-links ()
  "Adds additional CLHS entries as defined in *CLHS-ADD-ONS*."
  (loop for (entry link) in *clhs-add-ons*
        do (add-clhs-entry entry link)))

(defun collect-mop-links ()
  "Adds MOP entries as defined by the fragments in *MOP-LINKS*."
  (let ((mop-url (make-file-url *mop-page*)))
    (loop for (entry link) in *mop-links*
          do (add-doc-entry entry (format nil "~A~A" mop-url link)))))

(defun setup-doc-entries ()
  "Empties *DOC-HASH* and then \(re-)fills it as described above.
Finally sets up *DOC-HASH-ENTRIES* as well."
  (clrhash *doc-hash*)
  (collect-mop-links)
  (collect-lw-links)
  (collect-clhs-links)
  (collect-clhs-add-on-links)
  (setq *doc-hash-entries*
        (loop for key being the hash-keys of *doc-hash*
              collect key)))

;; now do it
(setup-doc-entries)

(defun complete-doc-entry (string parse-inf)
  "Completion function used by \"Meta Documentation\" command."
  (declare (ignore parse-inf))
  (editor::complete-string string *doc-entries*
                           :ignore-case t))

(defun hyperdoc-lookup-function-and-base-uri (package)
  "If PACKAGE is a package with Hyperdoc support the lookup function
and the base URI are returned as two values."
  (let ((lookup-symbol (find-symbol "HYPERDOC-LOOKUP" package))
        (base-uri-symbol (find-symbol "*HYPERDOC-BASE-URI*" package)))
    (when (and lookup-symbol
               base-uri-symbol
               (fboundp lookup-symbol)
               (boundp base-uri-symbol))
      (values (symbol-function lookup-symbol)
              (symbol-value base-uri-symbol)))))

(defmethod do-hyperdoc-lookup ((symbol symbol))
  "Checks if SYMBOL has an associated Hyperdoc URI and returns it."
  (let ((package (symbol-package symbol)))
    (multiple-value-bind (lookup-function base-uri)
        (hyperdoc-lookup-function-and-base-uri package)
      (when-let (partial-uri (and lookup-function
                                  (or (funcall lookup-function symbol 'function)
                                      (funcall lookup-function symbol 'variable))))
        (string-append base-uri partial-uri)))))

(defmethod do-hyperdoc-lookup ((string string))
  "Applies DO-HYPERDOC-LOOKUP to all external symbols named STRING in
all packages with Hyperdoc support."
  (loop for package in (list-all-packages)
        for is-candidate = (hyperdoc-lookup-function-and-base-uri package)
        for (symbol status) = (multiple-value-list
                               (and is-candidate
                                    (find-symbol (string-upcase string) package)))
        for uri = (and symbol (eq status :external)
                       (do-hyperdoc-lookup symbol))
        when uri do (return uri)))

(defun collect-hyperdoc-entries ()
  "Collects a list of \(downcased) symbol names of all external
symbols in all packages with Hyperdoc support."
  (loop for package in (list-all-packages)
        when (hyperdoc-lookup-function-and-base-uri package)
        nconc (loop for symbol being the external-symbols of package
                    when (do-hyperdoc-lookup symbol)
                    collect (string-downcase (symbol-name symbol)))))
