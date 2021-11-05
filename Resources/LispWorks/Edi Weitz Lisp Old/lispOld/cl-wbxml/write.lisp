;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/write.lisp,v 1.14 2007/01/01 23:44:09 edi Exp $

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

(defun get-namespace-prefix (uri &optional adder)
  "Returns a namespace prefix associated with the namespace URI
URI.  If there is no association \(in *NAMESPACES*) a new one is
created so that the function returns the same value for the same
URI on the next call.  If ADDER is not NIL it is supposed to be a
function which is called whenever a new prefix is constructed."
  (or (cdr (assoc uri *namespaces* :test #'string=))
      (let ((new-prefix (format nil "pre~A" (incf *namespace-counter*))))
        (push (cons uri new-prefix) *namespaces*)
        (when adder
          (funcall adder (format nil "xmlns:~A" new-prefix)))
        new-prefix)))

(defun get-prefixed-name (qname &optional adder)
  "If QNAME is a string it is returned.  If QNAME is a
namespace-qualified name \(in pseudo-XMLS syntax) a corresponding
prefixed name is returned.  The function always returns the same
prefix for the same namespace."
  (cond ((stringp qname) qname)
        (t (format nil "~A:~A"
                   (get-namespace-prefix (cdr qname) adder)
                   (car qname)))))

(defun build-string-list (document string-list prefer-inline)
  "Parses the XML element DOCUMENT \(in pseudo-XMLS format) and
adds strings to the list STRING-LIST if these are necessary to
encode the element \(and not already in the list).  Tries to add
as few as possible strings to the list if PREFER-INLINE is true."
  ;; the strategy used here and in WRITE-ATTRIBUTES isn't optimal
  ;; because we only check "from right to left" and ignore fragments
  ;; in the middle, but hey...
  (labels ((add-string (string)
             (pushnew string string-list :test #'equal))
           (add-name (qname)
             (cond ((stringp qname) (add-string qname))
                   (t (add-string (get-prefixed-name qname #'add-string))))))
    (cond ((stringp document) (unless prefer-inline
                                (add-string document)))
          ((typep (first document) 'octet))
          (t (let ((element-name (xmls-name document)))
               (unless (find-tag-token element-name)
                 (add-name element-name)))
             (loop for (name value) in (xmls-attributes document)
                   for value-length = (length value)
                   ;; try to find the longest possible prefix (if any)
                   ;; in the code pages
                   for prefix = (loop for end downfrom value-length to 0
                                      when (find-attr-start-token name
                                                                  (subseq value 0 end))
                                      return end
                                      ;; not found, so add it
                                      finally (add-name name)
                                              (return 0))
                   ;; now try to find value fragments as long as possible
                   do (loop for start = prefix
                            then (loop for end downfrom value-length to start
                                       when (find-attr-value-token (subseq value start end))
                                       return end
                                       ;; add string if no fragment found
                                       finally (unless prefer-inline
                                                 (add-string (subseq value start value-length)))
                                               (return nil))
                            while start))
             ;; now the function recursively calls itself to deal with
             ;; the children of this element
             (dolist (child (xmls-children document))
               (setq string-list (build-string-list child string-list prefer-inline))))))
  string-list)

(defun write-version (stream major minor)
  "Writes the WBXML version denoted by the integers MAJOR and
MINOR to the stream STREAM."
  (write-byte (logior (ash (1- major) 4) minor) stream))

(defun write-multi-byte-integer (stream value)
  "Writes the integer VALUE to the stream STREAM as a WBXML
multi-byte integer."
  (dolist (octet (nreverse
                  (loop for high = 0 then #b10000000
                        for rest = value then (ash value -7)
                        for masked-rest = (logand rest #b01111111)
                        collect (logior masked-rest high)
                        until (= rest masked-rest))))
    (write-byte octet stream)))

(defun write-publicid (stream publicid &optional force-literal)
  "Writes the public ID PUBLICID \(a string) to the stream
STREAM.  Uses code numbers of well-known public IDs if possible
unless FORCE-LITERAL is true.  Returns a true value if a string
index \(0) was written and thus the public ID string must be
prepended to the string table."
  (let ((token (and (not force-literal)
                    (car (rassoc publicid +publicids+ :test #'string=)))))
    (cond ((null publicid) (write-byte 1 stream) nil)
          (token (write-multi-byte-integer stream token) nil)
          (t (write-byte 0 stream) (write-byte 0 stream) t))))

(defun write-charset (stream charset)
  "Writes the charset CHARSET to the stream STREAM.  Signals an
error if a MIB enum encoding for this charset is not known to
CL-WBXML."
  (let* ((charset (external-format-name (make-external-format charset)))
         (token (car (rassoc charset +mib-enum-map+ :test #'eq))))
    (unless token
      (error "Don't know how to write charset ~S to WBXML stream." charset))
    (write-multi-byte-integer stream token)))

(defun write-string-table (stream external-format string-list)
  "Writes a WBXML string table to the stream STREAM consisting of
all strings in the list STRING-LIST and using the external format
EXTERNAL-FORMAT.  Returns a hash table which maps the strings to
their index w.r.t. the string table."
  (let* ((string-table (make-hash-table :test #'equal))
         ;; write data to a sequence first in order to compute
         ;; the length in octets and the indexes
         (string-table-length
          (length
           (with-output-to-sequence (out%)
             (let ((out (make-flexi-stream out% :external-format external-format)))
               (dolist (string string-list)
                 (setf (gethash string string-table)
                       (output-stream-sequence-length out%))
                 (write-string string out)
                 (write-char #\Null out)))))))
    (write-multi-byte-integer stream string-table-length)
    (dolist (string string-list)
      (write-string string stream)
      (write-char #\Null stream))
    string-table))

(defun get-string-index (string)
  "Returns the corresponding index if STRING is in the string
table, or NIL otherwise."
  (gethash string *string-table*))

(defun write-name-index (stream name)
  "Writes the name NAME to the stream STREAM where NAME can be a
string or a namespace-qualified name in pseudo-XMLS format.  NAME
MUST be found in *STRING-TABLE*."
  (write-multi-byte-integer stream
                            (get-string-index
                             (cond ((stringp name) name)
                                   (t (get-prefixed-name name))))))

(defun write-string* (stream string)
  "Writes the string STRING to the stream STREAM.  Uses STR_T if
possible, i.e. if STRING can be found in *STRING-TABLE*."
  (let ((index (get-string-index string)))
    (cond (index
           (write-byte +str-t+ stream)
           (write-multi-byte-integer stream index))
          (t (write-byte +str-i+ stream)
             (write-string string stream)
             (write-char #\Null stream)))))

(defun write-opaque (stream octet-list)
  "Writes the list of octets OCTET-LISTS to the stream STREAM as
opaque data."
  (write-byte +opaque+ stream)
  (write-multi-byte-integer stream (length octet-list))
  (dolist (octet octet-list)
    (write-byte octet stream)))

(defun write-content (stream thing)
  "Writes the XML content THING to the stream STREAM where THING
can be a string, a list of octets, or an XML element."
  (cond ((stringp thing) (write-string* stream thing))
        ((typep (first thing) 'octet)
         (write-opaque stream thing))
        (t (write-element stream thing))))

(defun write-token (stream token code-page)
  "Writes the token TOKEN from the code page CODE-PAGE to the
stream STREAM.  Writes a SWITCH_PAGE instruction first if
necessary."
  (maybe-switch-code-page stream code-page)
  (write-byte token stream))

(defun write-attributes (stream attributes)
  "Writes the list of attributes ATTRIBUTES - encoded as \(NAME
VALUE) - to the stream STREAM."
  (loop with *state* = :attr
        for (name value) in attributes
        for value-length = (length value)
        ;; try to find the longest possible prefix (if any) in the
        ;; code pages
        for prefix = (loop for end downfrom value-length to 0
                           ;; multiple-value-bind => list => destructuring-bind
                           ;; you think that's ugly?
                           for (token code-page) = (multiple-value-bind (token code-page)
                                                       (find-attr-start-token name (subseq value 0 end))
                                                     (list token code-page))
                           when token
                           do (write-token stream token code-page)
                           and return end
                           ;; no match, so write attribute name as literal
                           finally (write-byte +literal+ stream)
                                   (write-name-index stream name)
                                   (return 0))
                   ;; now try to find value fragments as long as possible
                   do (loop for start = prefix
                            then (loop for end downfrom value-length to start
                                       for (token code-page) = (multiple-value-bind (token code-page)
                                                                   (find-attr-value-token (subseq value start end))
                                                                 (list token code-page))
                                       when token
                                       do (write-token stream token code-page)
                                       and return end
                                       ;; nothing found, so write the rest as a string
                                       finally (write-string* stream (subseq value start value-length))
                                               (return nil))
                            while start)))

(defun write-element (stream element &optional root)
  "Writes the pseudo-XMLS element ELEMENT to the stream STREAM.
If ROOT is true namespace attributes are also written if
necessary."
  (let ((name (xmls-name element))
        (attributes (xmls-attributes element)))
    (when root
      ;; if this is the root element add the namespace declarations to
      ;; the list of attributes
      (loop for (uri . prefix) in *namespaces*
            do (push (list (format nil "xmlns:~A" prefix)
                           uri)
                     attributes)))
    (let ((*state* :tag)
          ;; the upper two bits decode whether there are attributes
          ;; and/or children
          (mask (cond ((and attributes
                            (xmls-children element))
                       #b11000000)
                      (attributes #b10000000)
                      ((xmls-children element) #b01000000)
                      (t 0))))
      (multiple-value-bind (token code-page)
          (find-tag-token name)
        (cond (token (write-token stream (logior token mask) code-page))
              (t (write-byte (logior +literal+ mask) stream)
                 (write-name-index stream name)))))
    (when attributes
      (write-attributes stream attributes)
      (write-byte +end+ stream))
    (when (xmls-children element)
      (dolist (child (xmls-children element))
        (write-content stream child))
      (write-byte +end+ stream))))

(defmethod unparse-wbxml (document (stream stream) &key publicid
                                                        force-literal-publicid
                                                        prefer-inline
                                                        (major-version 1)
                                                        (minor-version 3)
                                                        version-string
                                                        (if-exists :error)
                                                        (charset :utf8)
                                                        ((:tag-tokens *tag-tokens*))
                                                        ((:attr-tokens *attr-tokens*)))
  "Encodes the XML document DOCUMENT \(in XMLS-like syntax) as WBXML
and writes it to the binary/bivalent stream STREAM.  MAJOR-VERSION and
MINOR-VERSION \(integers) denote the WBXML version which should be
used.  VERSION-STRING \(a string) is another way to specifiy the
version and if this value is not NIL the other version paramters are
ignored.  PUBLICID is the public ID \(a string) of the document.  If
FORCE-LITERAL-PUBLICID is true the public ID is inserted as an index
into the string table even if there's a well-known numeric value for
it.  CHARSET is the character set that is to be used to encode the
document.  TAG-TOKENS and ATTR-TOKENS are lists of code pages \(and
they are ignored for public IDs known to CL-WBXML).  If PREFER-INLINE
is true STR_I is used instead of STR_T whenever possible.  IF-EXISTS
is ignored."
  (declare (ignore if-exists))
  ;; maybe overwrite values provided by user
  (maybe-set-token-tables publicid)
  (let* ((*namespace-counter* 0)
         (*namespaces* nil)
         (string-list (build-string-list document nil prefer-inline))
         (*state* :tag)
         (*tag-code-page* 0)
         (*attr-code-page* 0)
         (*namespace-stack* nil)
         (external-format (make-external-format charset :eol-style :lf))
         (out (make-flexi-stream stream :external-format external-format)))
    (when version-string
      ;; when there is a version string parse it and overwrite
      ;; MAJOR-VERSION and MINOR-VERSION
      (multiple-value-bind (major pos)
          (parse-integer version-string :junk-allowed t)
        (setq minor-version (parse-integer version-string :start (1+ pos))
              major-version major)))
    (write-version out major-version minor-version)
    (when (write-publicid out publicid force-literal-publicid)
      (push publicid string-list))
    (write-charset out charset)
    (let ((*string-table*
           (write-string-table out external-format string-list)))
      (write-element out document t))
    nil))

(defmethod unparse-wbxml (document (pathname pathname) &rest rest
                                                       &key publicid
                                                            force-literal-publicid
                                                            prefer-inline
                                                            (major-version 1)
                                                            (minor-version 3)
                                                            version-string
                                                            (if-exists :error)
                                                            (charset :utf8)
                                                            ((:tag-tokens *tag-tokens*))
                                                            ((:attr-tokens *attr-tokens*)))
  "Encodes the XML document DOCUMENT \(in XMLS-like syntax) as WBXML
and writes it to the file specified by PATHNAME.  MAJOR-VERSION and
MINOR-VERSION \(integers) denote the WBXML version which should be
used.  VERSION-STRING \(a string) is another way to specifiy the
version and if this value is not NIL the other version paramters are
ignored.  PUBLICID is the public ID \(a string) of the document.  If
FORCE-LITERAL-PUBLICID is true the public ID is inserted as an index
into the string table even if there's a well-known numeric value for
it.  CHARSET is the character set that is to be used to encode the
document.  TAG-TOKENS and ATTR-TOKENS are lists of code pages \(and
they are ignored for public IDs known to CL-WBXML).  If PREFER-INLINE
is true STR_I is used instead of STR_T whenever possible.  IF-EXISTS
is used when opening the file."
  (declare (ignore publicid force-literal-publicid prefer-inline
                   major-version minor-version version-string
                   charset *tag-tokens* *attr-tokens*))
  (with-open-file (out pathname :direction :output
                                :if-exists if-exists
                                :element-type 'octet)
    (apply #'unparse-wbxml document out rest)))
    
(defmethod unparse-wbxml (document (target (eql t)) &rest rest
                                                    &key publicid
                                                         force-literal-publicid
                                                         prefer-inline
                                                         (major-version 1)
                                                         (minor-version 3)
                                                         version-string
                                                         (if-exists :error)
                                                         (charset :utf8)
                                                         ((:tag-tokens *tag-tokens*))
                                                         ((:attr-tokens *attr-tokens*)))
  "Encodes the XML document DOCUMENT \(in XMLS-like syntax) as WBXML
and returns a vector with the corresponding octets.  MAJOR-VERSION and
MINOR-VERSION \(integers) denote the WBXML version which should be
used.  VERSION-STRING \(a string) is another way to specifiy the
version and if this value is not NIL the other version paramters are
ignored.  PUBLICID is the public ID \(a string) of the document.  If
FORCE-LITERAL-PUBLICID is true the public ID is inserted as an index
into the string table even if there's a well-known numeric value for
it.  CHARSET is the character set that is to be used to encode the
document.  TAG-TOKENS and ATTR-TOKENS are lists of code pages \(and
they are ignored for public IDs known to CL-WBXML).  If PREFER-INLINE
is true STR_I is used instead of STR_T whenever possible.  IF-EXISTS
is used when opening the file."
  (declare (ignore publicid force-literal-publicid prefer-inline
                   major-version minor-version version-string
                   charset if-exists *tag-tokens* *attr-tokens*))
  (with-output-to-sequence (out)
    (apply #'unparse-wbxml document out rest)))
    
