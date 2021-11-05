;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-DOC; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-doc/pages.lisp,v 1.12 2015/05/29 18:21:33 edi Exp $

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

(defun make-link (shortcut link)
  "Creates a HTML link for the saved link LINK and the shortcut
SHORTCUT.  Depends on the value of *LINK-PREFIX*."
  (format nil "~A~A~A"
          (case *link-prefix*
            ((nil) "../")
            ((t) *lw-link-prefix*)
            (otherwise *link-prefix*))
          (regex-replace-all "\\\\"
                             (directory-namestring (second (assoc shortcut *index-pages* :test #'string=)))
                             "/")
          link))

(defun create-char-list ()
  "Creates a list of links for the characters which appear in the
permuted index."
  (with-html-output (*html-stream*)
    (:p
     (dotimes (i 26)
       (let ((char (code-char (+ #.(char-code #\a) i))))
         (when (gethash char *sorted-table*)
           (htm
            (:a :href (file-namestring (make-file-name char))
                (str (string-upcase char)))
            "&nbsp;"))))
     (when (gethash #\* *sorted-table*)
       (htm
        (:a :href (file-namestring (make-file-name #\*))
            "Non-Alphabetic"))))))

(defun create-table (char)
  "Creates the HTML table with the permuted index for the
character CHAR."
  (with-html-output (*html-stream*)
    (loop for (symbol index type link-info) in (gethash char *sorted-table*)
          for left = (escape-and-fill-spaces symbol 0 index)
          for right = (escape-and-fill-spaces symbol index) do
            (labels ((write-part (string)
                       (ecase type
                         (bold
                          (htm (:b (str string))))
                         (code
                          (htm (:code (str string))))))
                     (write-link (string)
                       (cond ((null (cdr link-info))
                              (destructuring-bind (shortcut page-number link)
                                  (first link-info)
                                (htm
                                 (:a :href (make-link shortcut link)
                                     :title (format nil "~A-~A" shortcut page-number)
                                     (write-part string)))))
                              (t (write-part string)))))
              (htm
               (:tr
                (:td :align "right"
                     (write-link left))
                (:td :align "left"
                     (write-link right)
                     (when (cdr link-info)
                       (htm "&nbsp;&nbsp;&nbsp;["
                            (loop for (shortcut page-number link) in (sort (copy-list link-info)
                                           (lambda (link-entry-1 link-entry-2)
                                             (or (string< (first link-entry-1)
                                                          (first link-entry-2))
                                                 (and (string= (first link-entry-1)
                                                               (first link-entry-2))
                                                      (< (second link-entry-1)
                                                         (second link-entry-2))))))
                                  for spaces = "" then "&nbsp;" do
                                    (htm
                                      (str spaces)
                                      (:a :href (make-link shortcut link)
                                          (:em (fmt "~A-~A" shortcut page-number)))))
                            "]")))))))))

(defun create-page (&optional char)
  "Creates the index page for the character CHAR or the start
page if CHAR is NIL."
  (with-open-file (*html-stream* (make-file-name char)
                   :direction :output
                   :if-exists :supersede)
    (with-html-output (*html-stream*)
      (flet ((make-title ()
               (htm
                (fmt "Permuted Index for ~A ~A Docs~@[ - ~A~]"
                     (lisp-implementation-type)
                     (lisp-implementation-version)
                     (case char
                       ((nil) nil)
                       (#\* "Non-Alphabetic")
                       (otherwise (char-upcase char)))))))
        (htm
         (:html
          (:head
           (:title (make-title))
           (:meta :name "author" :content "Dr. Edmund Weitz, Hamburg, Germany")
           (:meta :name "copyright" :content "Dr. Edmund Weitz, Hamburg, Germany")
           (:style :type "text/css"
                   "* { font-size: 10pt; font-weight: bold; font-family: Verdana, Arial, Helvetica, Geneva, sans-serif; }
code { font-family: Courier; }
em { font-size: 8pt; font-weight: medium; }
h2 { font-size: 12pt; } "))
          (:body
           (:h2 (make-title))
           (create-char-list)
           (when char
             (htm
              (:p
               (:table :border 0 :cellspacing 0 :cellpadding 0
                       (create-table char)))
              (create-char-list))))))))))

(defun create-permuted-index (&key ((:link-prefix *link-prefix*) nil)
                                   ((:target-dir *target-dir*) *docs-base-path*))
  "Creates a directory called `permuted-index' which contains a
file `index.html' and several other files linked from there which
together comprise a permuted index for \(parts of) the LispWorks
documentation.  By default the directory is created in the same
directory where the LW browsable documentation can be found but
this can be changed by providing the TARGET-DIR keyword
parameter.  By default the links are created relative to the
afore-mentioned default directory but you can provide an
arbitrary prefix string through the keyword parameter
LINK-PREFIX.  If this parameter is T the index entries are linked
to the documentation found at the LispWorks website."
  (parse-files)
  (clrhash *sorted-table*)
  (fill-sorted-table)
  (sort-sorted-table-rows)
  (loop for char being the hash-keys of *sorted-table* do
    (create-page char))
  (create-page))
