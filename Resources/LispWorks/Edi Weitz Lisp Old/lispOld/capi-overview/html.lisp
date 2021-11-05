;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CAPI-OVERVIEW; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/capi-overview/html.lisp,v 1.10 2007/01/01 23:38:50 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz. All rights reserved.

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

(in-package :capi-overview)

(defun init-lw-doc ()
  "Calls into LW-DOC module to populate its *LINK-TABLE* hash
table."
  (lw-doc:parse-files))

(defun init-capi-links ()
  "Populates *CAPI-LINK-TABLE* with values determined by LW-DOC."
  (init-lw-doc)
  (clrhash *capi-link-table*)
  (loop for (symbol nil) being the hash-keys of lw-doc::*link-table*
        using (hash-value link-infos)
        for pos = (position "CAPI" link-infos :test #'string= :key #'first)
        when pos
          do (setf (gethash symbol *capi-link-table*)
                     (third (nth pos link-infos)))))

(defun style-sheet ()
  "Returns the CSS style sheet used for the HTML page which
depends on the value of *FONT-SIZE*."
  (format nil "
  * { font-size: ~Apt; font-family: Verdana, Geneva, Helvetica, sans-serif; }
  a { text-decoration: none; color: black; }
  a { border:1px solid white; }
  a:hover   { border: 1px solid black; } 
  .normal { border: 1px solid white; }
  .direct_sup { border: 1px solid red; background-color: red; color: white; }
  .sup { border: 1px solid red; color: red; }
  .direct_sub { border: 1px solid green; background-color: green; color: white; }
  .sub { border: 1px solid green; color: green; }
" *font-size*))

(defun js-name (capi-class)
  "Returns a name for the CAPI-CLASS object CAPI-CLASS which is
suitable for Javascript."
  (regex-replace-all "[^a-z]"
                     (string-downcase (name capi-class))
                     "_"))

(defun display-name (capi-class)
  "Returns a name for the CAPI-CLASS object CAPI-CLASS which is
suitable for HTML display."
  (let ((name (name capi-class)))
    (format nil "~:[gp:~;~]~A"
            (eq (symbol-package name) #.(find-package :capi))
            (string-downcase name))))

(defun mouse-over (capi-class)
  "Returns the `MouseOver' code for the CAPI-CLASS object
CAPI-CLASS, i.e. the code which sets the CSS styles of all
subclasses and superclasses accordingly."
  (let ((name (name capi-class)))
    (format nil "~{change_style(\"~A\",\"~A\");~}"
            (nconc
             (loop with superclasses = (superclasses name)
                   for super in (superclasses* name)
                   for super-capi-class = (capi-class super)
                   collect (js-name super-capi-class)
                   collect (cond ((member (name super-capi-class)
                                          superclasses :test #'eq)
                                  "direct_sup")
                                 (t "sup")))
             (loop with subclasses = (subclasses name)
                   for sub in (subclasses* name)
                   for sub-capi-class = (capi-class sub)
                   collect (js-name sub-capi-class)
                   collect (cond ((member (name sub-capi-class)
                                          subclasses :test #'eq)
                                  "direct_sub")
                                 (t "sub")))))))

(defun mouse-out (capi-class)
  "Returns the `MouseOut' code for the CAPI-CLASS object
CAPI-CLASS, i.e. the code which sets the CSS styles of all
subclasses and superclasses back to `normal'."
  (format nil "~{change_style(\"~A\",\"~A\");~}"
          (loop with name = (name capi-class)
                for sub/super in (nconc (superclasses* name)
                                        (subclasses* name))
                for sub/super-capi-class = (capi-class sub/super)
                collect (js-name sub/super-capi-class)
                collect "normal")))

(defun create-capi-overview (&key ((:link-prefix lw-doc:*link-prefix*) nil)
                                  ((:target-dir lw-doc:*target-dir*) lw-doc:*docs-base-path*))
  "Creates a directory called `capi' which contains a file
`index.html' which provides a graphical overview of \(some) CAPI
and GP classes.  By default the directory is created in the same
directory where the LispWorks browsable documentation can be
found but this can be changed by providing the TARGET-DIR keyword
parameter.  By default links to the LW documentation are created
relative to the afore-mentioned default directory but you can
provide an arbitrary prefix string through the keyword parameter
LINK-PREFIX.  If this parameter is T the entries are linked to
the documentation found at the LispWorks website."
  (with-open-file (*html-stream* (lw-doc:make-file-name nil "capi")
                                 :direction :output
                                 :if-exists :supersede)
    (with-html-output (*html-stream*)
      (:html
       (:head
        (:meta :name "author" :content "Dr. Edmund Weitz, Hamburg, Germany")
        (:meta :name "copyright" :content "Dr. Edmund Weitz, Hamburg, Germany")
        (:title "Overview of CAPI/GP Classes")
        (:style :type "text/css" (esc (style-sheet)))
                
        (:script :language "Javascript"
                 "
  function change_style(id, newClass) {
    target = document.getElementById(id);
    target.className = newClass;
    return true;
  }
"))
       (:body
        (:table :border 0 :cellspacing *cellspacing* :cellpadding *cellpadding*
                (write-html-table)))))))

(defun write-html-table ()
  "Utility function used by CREATE-CAPI-OVERVIEW to create the
actual HTML table."
  (init-capi-links)
  (let ((all-capi-classes (copy-list (all-capi-classes))))
    (loop
      (with-html-output (*html-stream*)
        (:tr
          (unless all-capi-classes
            (return-from write-html-table))
          (dotimes (level (1+ (loop for capi-class in *all-capi-classes*
                                    maximize (level capi-class))))
            (htm
             (let ((class-with-level (find level all-capi-classes :key #'level)))
               (cond (class-with-level
                      (setq all-capi-classes (remove class-with-level all-capi-classes))
                      (let ((link-fragment (gethash (symbol-name (name class-with-level)) *capi-link-table*)))
                        (htm (:td (:a :href (cond (link-fragment
                                                   (lw-doc:make-link "CAPI" link-fragment))
                                                  (t nil))
                                      :id (js-name class-with-level)
                                      :onmouseover (mouse-over class-with-level)
                                      :onmouseout (mouse-out class-with-level)
                                      (esc (display-name class-with-level)))))))
                     (t (htm (:td))))))))))))
