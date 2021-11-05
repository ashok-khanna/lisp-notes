;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/url-rewrite/test.lisp,v 1.9 2007/01/01 23:55:43 edi Exp $

;;; Copyright (c) 2004-2007, Dr. Edmund Weitz. All rights reserved.

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

(in-package #:url-rewrite)

(defvar +session-cookie-name+ "session")

(defun add-session-var (html session-value)
  (with-input-from-string (*standard-input* html)
    (with-output-to-string (*standard-output*)
      (rewrite-urls (lambda (url)
		      (add-get-param-to-url (or url "/")
					    +session-cookie-name+
					    session-value))))))

;; some simple test cases - there should be more... :)
(defparameter *test-cases*
  '(("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\"> 
<HTML>
<BODY BGCOLOR=white>
This is the <A NAME=foo HREF=\"first.html\">first link</A>, and here's the <A HREF=\"mailto:bill@microsoft.com\" TITLE='bar'>second one</A>.
</BODY>
</HTML>"
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\"> 
<HTML>
<BODY BGCOLOR=white>
This is the <A NAME=foo HREF='first.html?session=foo42'>first link</A>, and here's the <A HREF=\"mailto:bill@microsoft.com\" TITLE='bar'>second one</A>.
</BODY>
</HTML>")
    ("Just some plain text")
    ;; error in comment declaration
    ("<A HREF='/frob'>outer link</A> <!-- comment with embedded <A HREF='/frob'>link</A>-- --another comment-- error <a href=\"/foo/frob/bar.html\">...</a>"
     "<A HREF='/frob?session=foo42'>outer link</A> <!-- comment with embedded <A HREF='/frob'>link</A>-- --another comment-- error <a href='/foo/frob/bar.html?session=foo42'>...</a>")
    ;; wrong comment declaration
    ("<!--------->"
     "<!-------->")
    ;; correct comment declaration
    ("<!-------->"
     "<!-------->")
    ("<% <a href=foo.html><img title='howdy' src=foo.gif border=0/></a>"
     "<% <a href='foo.html?session=foo42'><img title='howdy' src='foo.gif?session=foo42' border=0/></a>")
    ("<FORM NAME=Name-of-the-Form ACTION='/'><input type=text name=foo><br><input src=frob.gif value='Press me'></form>"
     "<FORM NAME=Name-of-the-Form ACTION='/?session=foo42'><input type=text name=foo><br><input src='frob.gif?session=foo42' value='Press me'></form>")
    ("<form name=name-of-the-form><input type=text name=foo><br><input src=frob.gif value='Press me'></form>"
     "<form name=name-of-the-form action='/?session=foo42'><input type=text name=foo><br><input src='frob.gif?session=foo42' value='Press me'></form>")))

(defun test ()
  (loop for (input output) in *test-cases*
        for expected-output = (or output input)
        for i from 1
        for result = (add-session-var input "foo42")
        do (format t "~&Test #~A...~%" i)
           (force-output)
        unless (string= expected-output
                        result)
        do (format t "~&Test #~A failed - mismatch at position ~A!!~%~a~%~a"
                   i (mismatch expected-output result :test #'char=)
                   expected-output result))
  (values))