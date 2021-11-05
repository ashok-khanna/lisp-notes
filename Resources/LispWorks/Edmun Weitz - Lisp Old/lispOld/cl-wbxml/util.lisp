;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/util.lisp,v 1.18 2007/01/01 23:44:09 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :cl-wbxml)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:when-let))

#-:lispworks
(defmacro when-let ((var expr) &body body)
  "Evaluates EXPR, binds it to VAR, and executes BODY if VAR has
a true value."
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defun global-token-p (octet)
  "Checks whether OCTET can represent a global token."
  (< (logand octet #b111111) 5))

(defun attr-start-p (octet)
  "Checks whether OCTET can represent an ATTRSTART token."
  (and (not (global-token-p octet))
       (< octet 128)))

(defun starts-with-p (seq prefix &key (test #'char=))
  "Returns a true value if the sequence SEQ starts with the
sequence PREFIX whereby the elements are compared using TEST."
  (let ((mismatch (mismatch seq prefix :test test)))
    (or (null mismatch)
        (= mismatch (length prefix)))))

(defun switch-page (stream)
  "Reads the next octet from STREAM and switches the
corresponding code page depending on the parser state.  It is
assumed that the SWITCH_PAGE token has already been consumed."
  (let ((new-code-page (read-byte stream)))
    (when (= new-code-page 255)
      (error "Code page 255 is reserved (at position ~A)."
             (1- (flexi-stream-position stream))))
    (case *state*
      (:tag (setq *tag-code-page* new-code-page))
      (:attr (setq *attr-code-page* new-code-page)))))

(defun maybe-switch-code-page (stream new-code-page)
  "Unless NEW-CODE-PAGE is already the current code page \(in the
current context) writes the corresponding switch instruction to
the stream STREAM."
  (let ((current-code-page (ecase *state*
                             (:tag *tag-code-page*)
                             (:attr *attr-code-page*))))
    (unless (= current-code-page new-code-page)
      (write-byte +switch-page+ stream)
      (write-byte new-code-page stream)
      (case *state*
        (:tag (setq *tag-code-page* new-code-page))
        (:attr (setq *attr-code-page* new-code-page))))))

(defun read-byte* (stream)
  "Like READ-BYTE but is prepared to encounter a SWITCH_PAGE
token and act accordingly."
  (let ((octet (read-byte stream)))
    (cond ((eq octet +switch-page+)
           (switch-page stream)
           (read-byte stream))
          (t octet))))

(defun peek-byte* (stream &optional peek-type eof-error-p)
  "Like PEEK-BYTE but is prepared to encounter a SWITCH_PAGE
token and act accordingly."
  (let ((octet (peek-byte stream peek-type eof-error-p)))
    (cond ((eq octet +switch-page+)
           (read-byte stream)
           (switch-page stream)
           (peek-byte stream peek-type eof-error-p))
          (t octet))))

(defun read-null-terminated-string (stream &optional (max most-positive-fixnum))
  "Reads a NULL-terminated string from the stream STREAM using
the stream's current external format."
  (with-output-to-string (out)
    (loop repeat max
          for char = (read-char stream)
          until (char= char #\Null)
          do (write-char char out))))

(defun maybe-set-token-tables (publicid)
  "Sets the values of the special variables *TAG-TOKENS* and
*ATTR-TOKENS* depending on the public ID PUBLICID \(if it can be found
in +KNOWN-TOKEN-TABLES+."
  (when-let (token-tables (cdr (assoc publicid +known-token-tables+
                                      :test #'equal)))
    (setq *tag-tokens* (first token-tables)
          *attr-tokens* (second token-tables))))

(defun find-tag-token (name)
  "Searches through *TAG-TOKENS* to find the tag denoted by NAME and
returns \(if it finds it) two values - the corresponding token and the
code page."
  (loop for (code-page . table) in *tag-tokens* do
        (loop for (token . token-name) in table
              when (equal name token-name)
              do (return-from find-tag-token
                   (values token code-page)))))

(defun find-attr-start-token (name value-prefix)
  "Searches through *ATTR-TOKENS* to find the attribute start denoted
by NAME and VALUE-PREFIX and returns \(if it finds it) two values -
the corresponding token and the code page."
  (loop with spec = (cons name
                          (cond ((string= value-prefix "") nil)
                                (t value-prefix)))
        for (code-page . table) in *attr-tokens* do
        (loop for (token . token-name) in table
              when (and (attr-start-p token)
                        (equal spec token-name))
              do (return-from find-attr-start-token
                   (values token code-page)))))

(defun find-attr-value-token (name)
  "Searches through *ATTR-TOKENS* to find the attribute value \(part)
denoted by NAME \(a string) and returns \(if it finds it) two values -
the corresponding token and the code page."
  (when (string= name "")
    (return-from find-attr-value-token))
  (loop for (code-page . table) in *attr-tokens* do
        (loop for (token . token-name) in table
              when (and (not (attr-start-p token))
                        (string= name token-name))
              do (return-from find-attr-value-token
                   (values token code-page)))))

(defun xmls-children (node)
  "Returns the children of a node which in XMLS are the third and
following elements of a list."
  (cddr node))

(defun (setf xmls-children) (new-value node)
  "Sets the children of a node which in XMLS are the third and
following elements of a list."
  (setf (cddr node) new-value))

(defun xmls-name (node)
  "Returns the element name of a node which in XMLS is the first
element of a list."
  (first node))

(defun (setf xmls-name) (new-value node)
  "Sets the element name of a node which in XMLS is the first
element of a list."
  (setf (first node) new-value))

(defun xmls-attributes (node)
  "Returns the list of attributes of a node which in XMLS is the
second element of a list."
  (second node))

(defun (setf xmls-attributes) (new-value node)
  "Sets the list of attributes of a node which in XMLS is the
second element of a list."
  (setf (second node) new-value))
