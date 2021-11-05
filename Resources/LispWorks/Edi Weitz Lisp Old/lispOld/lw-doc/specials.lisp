;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-DOC; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-doc/specials.lisp,v 1.27 2015/05/29 18:21:33 edi Exp $

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

(defvar *docs-base-path*
  (make-pathname :name nil
                 :type nil
                 :version nil
                 :defaults 
                 #+(or :lispworks6.1 :lispworks7)
                 (sys:lispworks-dir "manual/online/")
                 #-(or :lispworks6.1 :lispworks7)
                 (sys:lispworks-dir "manual/online/web/"))
  "A pathname denoting the directory where the browsable
documentation can be found.")

(defvar *index-pages*
  (flet ((find-highest-numbered-html-file (pattern)
           "Finds and returns the pathname with the highest number
following the last hyphen in its name of those files in
*DOCS-BASE-PATH* which match PATTERN \(if any)."
           (lw:when-let (path
                         (first (sort (directory (merge-pathnames pattern *docs-base-path*))
                                      #'>
                                      :key (lambda (pathname)
                                             (let* ((basename (pathname-name pathname))
                                                    (hyphen-pos (position #\- basename :from-end t)))
                                               (or (parse-integer (subseq (pathname-name pathname)
                                                                          (1+ hyphen-pos))
                                                                  :junk-allowed t)
                                                   -1000000))))))
             (regex-replace-all "\\\\" (enough-namestring path *docs-base-path*) "/"))))
    `(#+win32
      ("COM" ,(find-highest-numbered-html-file "COM/html/com-*.htm")) 
      ("ED" #+:win32 ,(find-highest-numbered-html-file "EDUG-W/html/eduser-w-*.htm")
            #+(or :linux :freebsd) ,(find-highest-numbered-html-file "EDUG-U/html/eduser-u-*.htm")
            #+:mac ,(find-highest-numbered-html-file "EDUG-M/html/eduser-m-*.htm"))
      ("DLV" ,(or (find-highest-numbered-html-file "DV/html/delivery-*.htm")
		  (find-highest-numbered-html-file "DV/html/deluser-*.htm")))
      ("FLI" ,(find-highest-numbered-html-file "FLI/html/fli-*.htm"))
      ("LW" ,(or (find-highest-numbered-html-file "LW/html/lw-*.htm")
		 (find-highest-numbered-html-file "LWRM/html/lwref-*.htm")))
      ("CAPI" ,(or (find-highest-numbered-html-file "CAPRM/html/capiref-*.htm")
                   #+:win32 (find-highest-numbered-html-file "CAPI-W/html/capi-w-*.htm")
                   #+(or :linux :freebsd) (find-highest-numbered-html-file "CAPI-U/html/capi-u-*.htm")
                   #+:mac (find-highest-numbered-html-file "CAPI-M/html/capi-m-*.htm")))))
  "An alist mapping shortcuts for LW documentation sections to
the relative location of their index page.")


(defvar *link-table* (make-hash-table :test #'equal)
  "A hash table which maps symbols and editor commands to lists
with link information.")

(defvar *sorted-table* (make-hash-table :test #'equal)
  "A hash table which maps characters to a list of all
\(permuted) index entries starting with the corresponding
character.")

(defconstant +html-entities+ '(("amp" . "&")
                               ("lt" . "<")
                               ("gt" . ">")
                               ("nbsp" . " "))
  "An alist mapping some names of HTML entities to the characters
they denote.")

(defvar *link-prefix* nil
  "During the computation of the permuted index this variable can
be bound to a prefix which will be added to each link.")

(defvar *lw-link-prefix*
  #+:lispworks4.4 "http://www.lispworks.com/documentation/lw445/"
  #+:lispworks5.0 "http://www.lispworks.com/documentation/lw50/"
  #+:lispworks5.1 "http://www.lispworks.com/documentation/lw51/"
  #+:lispworks6.0 "http://www.lispworks.com/documentation/lw60/"
  #+:lispworks6.1 "http://www.lispworks.com/documentation/lw61/"
  #+:lispworks7.0 "http://www.lispworks.com/documentation/lw70/"
  "The prefix for the LispWorks online documentation.")

(defvar *html-stream* nil
  "During the creation of the permuted index this variable is
bound to the stream the HTML content is written to.")

(defvar *target-dir* nil
  "During the computation of the permuted index this variable is
bound to a pathname denoting the directory where the index should
be created.")
