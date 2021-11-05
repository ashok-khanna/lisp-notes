;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/read.lisp,v 1.30 2007/01/01 23:44:09 edi Exp $

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

(defun read-version (stream)
  "Reads the version information from the stream STREAM and
returns it as a string.  Also checks for acceptable value and
signals an error otherwise."
  (let* ((octet (read-byte stream))
         (major (1+ (ash octet -4)))
         (minor (logand octet #b1111))
         (version-string (format nil "~A.~A" major minor)))
    (unless (and (= major 1)
                 (<= 1 minor 3))
      (error "Unknown WBXML version ~S (#x~2,'0X) at position ~A."
             version-string octet
             (1- (flexi-stream-position stream))))
    version-string))

(defun read-multi-byte-integer (stream)
  "Reads and returns a multi-byte integer from the stream STREAM
as defined by the WBXML spec."
  (loop for result = 0 then (+ (ash result 7)
                               (logand octet #b01111111))
        for octet = (read-byte stream)
        until (zerop (logand octet #b10000000))
        finally (return (+ (ash result 7) octet))))

(defun read-publicid (stream)
  "Reads the information pertaining to the XML document public
identifier from the stream STREAM.  Returns either a string, an
index into the string table \(which doesn't exists at this
point), or NIL."
  (let ((id (read-multi-byte-integer stream)))
    (when (<= #xe id #x7f)
      (error "Reserved public identifier #x~2,'0X at position ~A."
             id (1- (flexi-stream-position stream))))
    (case id
      ;; index into string table
      (0 (read-multi-byte-integer stream))
      ;; unknown or missing
      (1 nil)
      ;; look up in table of `well-known' public ids
      (otherwise (or (cdr (assoc id +publicids+))
                     (error "Unknown public identifier #x~2,'0X at position ~A."
                            id (1- (flexi-stream-position stream))))))))

(defun read-charset (stream)
  "Reads and interprets the information pertaining to the
character encoding from the stream STREAM.  Returns a keyword
corresponding to the encoding or NIL."
  (let* ((id (read-multi-byte-integer stream))
         (charset (cdr (assoc id +mib-enum-map+))))
    (cond ((zerop id) nil) ; unknown charset - use transport meta-information
          (charset)
          (t (error "Don't know what to do with MIB number ~A (at position ~A)."
                    id (1- (flexi-stream-position stream)))))))

(defun read-string-table (stream)
  "Reads the string table from the stream STREAM and returns it
as an alist indexed by octet position."
  (loop with length = (read-multi-byte-integer stream)
        with initial-position = (flexi-stream-position stream)
        for position = (- (flexi-stream-position stream) initial-position)
        while (< position length)
        collect (cons position
                      (read-null-terminated-string stream (- length position)))))

(defmethod get-indexed-string ((index integer))
  "Returns the string from the current string table which
corresponds to the octet position INDEX."
  ;; can we index into the middle of strings?
  ;; the spec isn't clear about that and we don't do it for the moment
  (or (cdr (assoc index *string-table*))
      (error "No string in string table for index #x~2,'0X." index)))

(defmethod get-indexed-string ((stream stream))
  "Reads an integer from the stream STREAM and returns the string
from the current string table which corresponds to this octet
position."
  (get-indexed-string (read-multi-byte-integer stream)))

(defun look-up-token (octet &key attr)
  "Returns the value corresponding to the token OCTET from the
current code page.  If ATTR is true we're currently in the
attribute code space, otherwise we're in the tag code space."
  (when (global-token-p octet)
    (error "Unexpected global token #x~2,'0X." octet))
  (unless attr
    ;; for tags mask bits which contain structure info
    (setq octet (logand #b111111 octet)))
  (or (cdr (assoc octet
                  (cdr 
                   (cond (attr (assoc *attr-code-page* *attr-tokens*))
                         (t (assoc *tag-code-page* *tag-tokens*))))))
      (error "Don't know what ~:[~A~*~;~*~A~] belongs to token #x~2,'0X."
             attr "tag name" "attribute prefix" octet)))

(defun look-up-attr-start (stream)
  "Reads an octet from the stream STREAM, interprets it as an
attribute start token and returns the corresponding value."
  (let ((octet (read-byte* stream)))
    (unless (attr-start-p octet)
      (error "Expected value less than #x80 instead of #x~2,'0X at position ~A."
             octet (1- (flexi-stream-position stream))))
    (look-up-token octet :attr t)))

(defun look-up-attr-value (stream)
  "Reads an octet from the stream STREAM, interprets it as an
attribute value token and returns the corresponding value."
  (let ((octet (read-byte* stream)))
    (when (attr-start-p octet)
      (error "Expected value greater than #x7f instead of #x~2,'0X at position ~A."
             octet (1- (flexi-stream-position stream))))
    (look-up-token octet :attr t)))

(defun look-up-tag (stream)
  "Reads an octet from the stream STREAM, interprets it as a tag
token and returns the corresponding value."
  (look-up-token (read-byte* stream)))

(defun read-attr-start (stream)
  "Reads and returns the start of an attribute from the stream
STREAM."
  (cond ((eq (peek-byte stream) +literal+)
         (read-byte stream)
         (list (get-indexed-string stream)))
        (t (look-up-attr-start stream))))

(defun read-string (stream &optional handler)
  "Reads and returns a string from the stream STREAM.  It is
assumed that the next octet is either the token STR_I or STR_T.
Calls the CHARACTERS method of the handler HANDLER unless HANDLER
is NIL."
  (let ((string
         (ecase (read-byte stream)
           (#.+str-i+
            (read-null-terminated-string stream))
           (#.+str-t+
            (get-indexed-string (read-multi-byte-integer stream))))))
    (when handler
      (characters handler string))
    string))
        
(defun read-extension (stream &optional handler)
  "Reads and returns document-type-specific extension data from
the stream STREAM.  It is assumed that the octet is one of the
EXT-<foo> tokens.  Calls the CHARACTERS method of the handler
HANDLER unless HANDLER is NIL.  Uses *EXTENSION-FUNCTION* to
interpret what's read from the stream."
  (let* ((octet (read-byte stream))
         (id (logand octet #b11))
         (value
          (ecase (logand octet #b11000000)
            (#.+ext-0+ nil)
            (#.+ext-i-0+ (read-null-terminated-string stream))
            (#.+ext-t-0+ (read-multi-byte-integer stream))))
         (result (funcall *extension-function* id value)))
    (when handler
      (characters handler result))
    result))

(defun read-entity (stream &optional handler)
  "Reads a character entity from the stream STREAM and returns it
as a string of length one.  Calls the CHARACTERS method of the
handler HANDLER unless HANDLER is NIL.  It is assumed that the
next octet is the ENTITY token."
  (read-byte stream)
  (let* ((char (code-char (read-multi-byte-integer stream)))
         (string (make-string 1 :initial-element char)))
    (when handler
      (characters handler string))
    string))

(defun read-opaque (stream &optional handler)
  "Reads opaque data from the stream STREAM and returns it as a
list of octets.  Calls the CHARACTERS method of the handler
HANDLER unless HANDLER is NIL.  It is assumed that the next octet
is the OPAQUE token."
  (read-byte stream)
  (let ((octets (loop repeat (read-multi-byte-integer stream)
                      collect (read-byte stream))))
    (when handler
      (characters handler octets))
    octets))

(defun read-attr-value (stream)
  "Reads and returns \(a part of) an attribute value from the
stream STREAM."
  (case (peek-byte* stream)
    ((#.+str-i+ #.+str-t+)
     (read-string stream))
    (#.+ext-tokens+
     (read-extension stream))
    (#.+entity+
     (read-entity stream))
    (otherwise (look-up-attr-value stream))))

(defun read-pi (stream handler)
  "Reads a processing instruction from the stream STREAM.  Calls
the PROCESSING-INSTRUCTION method of the handler HANDLER.  It is
assumed that the next octet is the PI token."
  (read-byte stream)
  (let ((*state* :attr))
    (destructuring-bind (target &optional data-start)
        (read-attr-start stream)
      (let ((data (with-output-to-string (out)
                    (when data-start
                      (write-string data-start out))
                    (loop for octet = (peek-byte stream)
                          until (eq octet +end+)
                          collect (write-string (read-attr-value stream) out)))))
        ;; read END token
        (read-byte stream)
        (processing-instruction handler target data)))))

(defun read-attribute (stream)
  "Reads one attribute from the stream STREAM and constructs and
returns a corresponding ATTRIBUTE object.  Attributes dealing
with namespaces will be acted upon and returned as NIL."
  (destructuring-bind (qname . value-start)
      (read-attr-start stream)
    (let* ((value-rest
            ;; read value fragments until we see END, LITERAL, or
            ;; another attribute start token
            (loop for next-octet = (peek-byte stream)
                  until (or (member next-octet `(,+end+ ,+literal+))
                            (attr-start-p next-octet))
                  collect (read-attr-value stream)))
           (value (with-output-to-string (out)
                    (when value-start
                      (write-string value-start out))
                    (dolist (value-part value-rest)
                      (write-string value-part out)))))
      (cond ((and (stringp qname)
                  (starts-with-p qname "xmlns"))
             ;; handle namespace attributes
             (let ((prefix (and (> (length qname) 5)
                                (= (char qname 5) #\:)
                                (subseq qname 6))))
               (cond ((or (null value)
                          (zerop (length value)))
                      (setq *namespace-stack*
                            (remove prefix *namespace-stack*
                                    :key #'car
                                    :test #'equal)))
                     (t (push (cons prefix value) *namespace-stack*))))
             nil)
            (t (make-instance 'attribute
                              :qname qname
                              :value value))))))

(defun resolve-name (qname &key attr)
  "Converts the qualified name QNAME into two values - a
namespace URI and a local name.  If ATTR is true than QNAME is
the name of an attribute, otherwise it's the name of an element.
Note that QNAME can be a list \(denoting a name plus namespace
URI in XMLS format)."
  (cond ((stringp qname)
         (let* ((colon-pos (position #\: qname :test #'char=))
                (prefix (and colon-pos (subseq qname 0 colon-pos)))
                (local-name (cond (colon-pos (subseq qname (1+ colon-pos)))
                                  (t qname)))
                ;; for attributes there is no default namespace
                (namespace-uri (and (or (null attr)
                                        prefix)
                                    (cdr (assoc prefix *namespace-stack*
                                                :test #'equal)))))
           (when (and prefix
                      (null namespace-uri))
             (error "Unknown namespace prefix ~S." prefix))
           (values namespace-uri local-name qname)))
        (t (values (cdr qname) (car qname)))))

(defun read-attributes (stream)
  "Reads and returns all attributes belonging to one element from
the stream STREAM."
  (let ((*state* :attr)
        (attributes (loop until (eq (peek-byte stream) +end+)
                          collect (read-attribute stream))))
    ;; read END token
    (read-byte stream)
    ;; now (we couldn't do this earlier) resolve the qualified names
    ;; of the attributes if necessary
    (dolist (attribute attributes)
      (with-slots (namespace-uri local-name qname)
          attribute
        (multiple-value-setq (namespace-uri local-name qname)
            (resolve-name qname))))
    attributes))

(defun read-content (stream handler)
  "Reads the contents of an element from the stream STREAM and
invokes the methods of the handler HANDLER accordingly."
  (loop for next-octet = (peek-byte* stream)
        until (eq next-octet +end+)
        when (eq next-octet +pi+)
        do (read-pi stream handler)
        else do (case next-octet
                  ((#.+str-i+ #.+str-t+)
                   (read-string stream handler))
                  (#.+ext-tokens+
                   (read-extension stream handler))
                  (#.+entity+
                   (read-entity stream handler))
                  (#.+opaque+
                   (read-opaque stream handler))
                  (otherwise (read-element stream handler))))
  ;; read END token
  (read-byte stream))

(defun read-element (stream handler)
  "Reads an entire element from the stream STREAM and invokes the
methods of the handler HANDLER accordingly."
  (let* ((*namespace-stack* (copy-list *namespace-stack*))
         (start (peek-byte stream))
         (tag (cond ((member start
                             '(+literal+ +literal-c+ +literal-a+ +literal-ac+))
                     (read-byte stream)
                     (get-indexed-string stream))
                    (t (look-up-tag stream))))
         (attributes (when (plusp (logand start #b10000000))
                       (read-attributes stream))))
    (multiple-value-bind (namespace-uri local-name qname)
        (resolve-name tag)
      (start-element handler namespace-uri local-name qname attributes)
      (when (plusp (logand start #b1000000))
        (read-content stream handler))
      (end-element handler namespace-uri local-name qname))))

(defun read-body (stream handler)
  "Reads the body of a WBXML document from the stream STREAM and
invokes the methods of the handler HANDLER accordingly."
  (loop for octet = (peek-byte stream)
        while (eq octet +pi+)
        do (read-pi stream handler)
        finally (read-element stream handler))
  (loop for octet = (peek-byte stream nil nil)
        while (eq octet +pi+)
        do (read-pi stream handler)))

(defmethod parse-wbxml ((stream stream) handler &key (default-charset :utf8)
                                                     ((:tag-tokens *tag-tokens*))
                                                     ((:attr-tokens *attr-tokens*)))
  "Reads and parses a WBXML document from the stream STREAM \(it
must be possible to wrap a flexi stream around STREAM) and
invokes the methods of the handler HANDLER accordingly.
TAG-TOKENS and ATTR-TOKENS are lists of code pages,
DEFAULT-CHARSET is the one that's used if the document's charset
isn't specified.  Returns multiple values - the first value of
the final call to END-DOCUMENT, the public ID, the version \(as a
string), and the character set of the document."
  (let ((in (make-flexi-stream stream)))
    (setf (flexi-stream-position in) 0)
    (let* ((version (read-version in))
           (publicid (read-publicid in))
           (charset (read-charset in))
           ;; bind and initialize some special variables
           (*state* :tag)
           (*tag-code-page* 0)
           (*attr-code-page* 0)
           (*namespace-stack* nil))
      ;; set stream's external format according to charset
      (unless charset
        (setq charset (or default-charset
                          (error "Unknown or missing charset in~
WBXML document and no default was provided."))))
      (setf (flexi-stream-external-format in)
            (make-external-format charset :eol-style :lf))
      (let ((*string-table* (read-string-table in)))
        (when (numberp publicid)
          (setq publicid (get-indexed-string publicid)))
        ;; maybe overwrite values provided by user
        (maybe-set-token-tables publicid)
        (start-document handler)
        (read-body in handler)
        (values (end-document handler) publicid version charset)))))

(defmethod parse-wbxml ((pathname pathname) handler &key (default-charset :utf8)
                                                         ((:tag-tokens *tag-tokens*))
                                                         ((:attr-tokens *attr-tokens*)))
  "Reads and parses a WBXML document from the file specified by
PATHNAME and invokes the methods of the handler HANDLER
accordingly.  TAG-TOKENS and ATTR-TOKENS are lists of code pages
\(and they are ignored for public IDs known to CL-WBXML),
DEFAULT-CHARSET is the one that's used if the document's charset
isn't specified.  Returns multiple values - the first value of
the final call to END-DOCUMENT, the public ID, the version \(as a
string), and the character set of the document."
  (with-open-file (in pathname :element-type 'octet)
    (parse-wbxml in handler
                 :default-charset default-charset
                 :tag-tokens *tag-tokens*
                 :attr-tokens *attr-tokens*)))

(defmethod parse-wbxml ((vector vector) handler &key (default-charset :utf8)
                                                     ((:tag-tokens *tag-tokens*))
                                                     ((:attr-tokens *attr-tokens*)))
  "Reads and parses a WBXML document from the vector VECTOR \(which
must contain only octets) and invokes the methods of the handler
HANDLER accordingly.  TAG-TOKENS and ATTR-TOKENS are lists of code
pages \(and they are ignored for public IDs known to CL-WBXML),
DEFAULT-CHARSET is the one that's used if the document's charset isn't
specified.  Returns multiple values - the first value of the final
call to END-DOCUMENT, the public ID, the version \(as a string), and
the character set of the document."
  (with-input-from-sequence (in vector)
    (parse-wbxml in handler
                 :default-charset default-charset
                 :tag-tokens *tag-tokens*
                 :attr-tokens *attr-tokens*)))

(defmethod parse-wbxml ((list list) handler &key (default-charset :utf8)
                                                 ((:tag-tokens *tag-tokens*))
                                                 ((:attr-tokens *attr-tokens*)))
  "Reads and parses a WBXML document from the list LIST \(which must
contain only octets) and invokes the methods of the handler HANDLER
accordingly.  TAG-TOKENS and ATTR-TOKENS are lists of code pages \(and
they are ignored for public IDs known to CL-WBXML), DEFAULT-CHARSET is
the one that's used if the document's charset isn't specified.
Returns multiple values - the first value of the final call to
END-DOCUMENT, the public ID, the version \(as a string), and the
character set of the document."
  (with-input-from-sequence (in list)
    (parse-wbxml in handler
                 :default-charset default-charset
                 :tag-tokens *tag-tokens*
                 :attr-tokens *attr-tokens*)))
