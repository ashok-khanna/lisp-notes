;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/specials.lisp,v 1.17 2007/01/01 23:44:09 edi Exp $

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

(defconstant +mib-enum-map+
  '((1 . :other)
    (2 . :unknown)
    (3 . :ascii)
    (4 . :latin-1)
    (5 . :latin-2)
    (6 . :latin-3)
    (7 . :latin-4)
    (8 . :cyrillic)
    (9 . :arabic)
    (10 . :greek)
    (11 . :hebrew)
    (12 . :latin-5)
    (13 . :latin-6)
    (106 . :utf-8)
    (109 . :latin-7)
    (110 . :latin-8)
    (111 . :latin-0)
    (112 . :latin-10)
    (1000 . :unicode)
    (1000 . :ucs-4)
    (1013 . :utf-16be)
    (1014 . :utf-16le)
    (1015 . :utf-16)
    (2009 . :ibm850)
    (2010 . :ibm852)
    (2011 . :ibm437)
    (2013 . :ibm862)
    (2046 . :ibm855)
    (2047 . :ibm857)
    (2048 . :ibm860)
    (2049 . :ibm861)
    (2050 . :ibm863)
    (2051 . :ibm864)
    (2052 . :ibm865)
    (2054 . :ibm869)
    (2086 . :ibm866)
    (2250 . :windows-1250)
    (2251 . :windows-1251)
    (2252 . :windows-1252)
    (2253 . :windows-1253)
    (2254 . :windows-1254)
    (2255 . :windows-1255)
    (2256 . :windows-1256)
    (2257 . :windows-1257)
    (2258 . :windows-1258))
  "Maps some MIB enums to keyword symbols understood by FLEXI-STREAMS.
See <http://www.iana.org/assignments/character-sets>.")

(defconstant +switch-page+ #x0
  "Global token SWITCH_PAGE.")
(defconstant +end+ #x1
  "Global token END.")
(defconstant +entity+ #x2
  "Global token ENTITY.")
(defconstant +str-i+ #x3
  "Global token STR_I.")
(defconstant +literal+ #x4
  "Global token LITERAL.")
(defconstant +ext-i-0+ #x40
  "Global token EXT_I_0.")
(defconstant +ext-i-1+ #x41
  "Global token EXT_I_1.")
(defconstant +ext-i-2+ #x42
  "Global token EXT_I_2.")
(defconstant +pi+ #x43
  "Global token PI.")
(defconstant +literal-c+ #x44
  "Global token LITERAL_C.")
(defconstant +ext-t-0+ #x80
  "Global token EXT_T_0.")
(defconstant +ext-t-1+ #x81
  "Global token EXT_T_1.")
(defconstant +ext-t-2+ #x82
  "Global token EXT_T_2.")
(defconstant +str-t+ #x83
  "Global token STR_T.")
(defconstant +literal-a+ #x84
  "Global token LITERAL_A.")
(defconstant +ext-0+ #xc0
  "Global token EXT_0.")
(defconstant +ext-1+ #xc1
  "Global token EXT_1.")
(defconstant +ext-2+ #xc2
  "Global token EXT_2.")
(defconstant +opaque+ #xc3
  "Global token OPAQUE.")
(defconstant +literal-ac+ #xc4
  "Global token LITERAL_AC.")

(defconstant +ext-tokens+ `(,+ext-0+ ,+ext-1+ ,+ext-2+
                            ,+ext-1+ ,+ext-i-1+ ,+ext-t-1+
                            ,+ext-2+ ,+ext-i-2+ ,+ext-t-2+)
  "List of all tokens which apply to document-type-specific
extensions.")

(defconstant +publicids+
  '((#x2 . "-//WAPFORUM//DTD WML 1.0//EN")
    (#x3 . "-//WAPFORUM//DTD WTA 1.0//EN")
    (#x4 . "-//WAPFORUM//DTD WML 1.1//EN")
    (#x5 . "-//WAPFORUM//DTD SI 1.0//EN")
    (#x6 . "-//WAPFORUM//DTD SL 1.0//EN")
    (#x7 . "-//WAPFORUM//DTD CO 1.0//EN")
    (#x8 . "-//WAPFORUM//DTD CHANNEL 1.1//EN")
    (#x9 . "-//WAPFORUM//DTD WML 1.2//EN")
    (#xa . "-//WAPFORUM//DTD WML 1.3//EN")
    (#xb . "-//WAPFORUM//DTD PROV 1.0//EN")
    (#xc . "-//WAPFORUM//DTD WTA-WML 1.2//EN")
    (#xd . "-//WAPFORUM//DTD EMN 1.0//EN")
    (#xe . "-//OMA//DTD DRMREL 1.0//EN")
    (#xf . "-//WIRELESSVILLAGE//DTD CSP 1.0//EN")
    (#x10 . "-//WIRELESSVILLAGE//DTD CSP 1.1//EN")
    (#x11 . "-//OMA//DTD WV-CSP 1.2//EN")
    (#x12 . "-//OMA//DTD IMPS-CSP 1.3//EN")
    (#xfd1 . "-//SYNCML//DTD SyncML 1.0//EN")
    ;; also referred to as 1.1 - sigh...
    (#xfd2 . "-//SYNCML//DTD DevInf 1.0//EN")
    (#xfd3 . "-//SYNCML//DTD SyncML 1.1//EN")
    (#xfd4 . "-//SYNCML//DTD DevInf 1.1//EN")
    (#x1100 . "-//PHONE.COM//DTD ALERT 1.0//EN")
    (#x1101 . "-//PHONE.COM//DTD CACHE-OPERATION 1.0//EN")
    (#x1102 . "-//PHONE.COM//DTD SIGNAL 1.0//EN")
    (#x1103 . "-//PHONE.COM//DTD LIST 1.0//EN")
    (#x1104 . "-//PHONE.COM//DTD LISTCMD 1.0//EN")
    (#x1105 . "-//PHONE.COM//DTD CHANNEL 1.0//EN")
    (#x1106 . "-//PHONE.COM//DTD MMC 1.0//EN")
    (#x1107 . "-//PHONE.COM//DTD BEARER-CHOICE 1.0//EN")
    (#x1108 . "-//PHONE.COM//DTD WML 1.1//EN")
    (#x1109 . "-//PHONE.COM//DTD CHANNEL 1.1//EN")
    (#x110a . "-//PHONE.COM//DTD LIST 1.1//EN")
    (#x110b . "-//PHONE.COM//DTD LISTCMD 1.1//EN")
    (#x110c . "-//PHONE.COM//DTD MMC 1.1//EN")
    (#x110d . "-//PHONE.COM//DTD WML 1.3//EN")
    (#x110e . "-//PHONE.COM//DTD MMC 2.0//EN")
    (#x1200 . "-//3GPP2.COM//DTD IOTA 1.0//EN")
    (#x1201 . "-//SYNCML//DTD SyncML 1.2//EN")
    (#x1202 . "-//SYNCML//DTD MetaInf 1.2//EN")
    (#x1203 . "-//SYNCML//DTD DevInf 1.2//EN")
    (#x1204 . "-//NOKIA//DTD LANDMARKS 1.0//EN"))
  "A list which maps numbers to some `well-known' document type
public identifiers.")

(defvar *extension-function* (constantly nil)
  "A function to handle document-type-specific tokens like
EXT_I_1.  The function will be called with two arguments - an ID
\(one of 0, 1, or 2) and a value (a string, an integer, or
NIL).")

(defvar *string-table* nil
  "The current string table - bound during parsing and unparsing with
different semantics.")

(defvar *tag-code-page* 0
  "The current tag code page - bound by the parser.")
(defvar *attr-code-page* 0
  "The current attribute code page - bound by the parser.")

(defvar *tag-tokens* nil
  "The current list of tag token mappings - bound by the
parser.")
(defvar *attr-tokens* nil
  "The current list of attribute token mappings - bound by the
parser.")

(defvar *state* :tag
  "The current state while parsing and unparsing - one of :TAG or
:ATTR.")

(defvar *namespace-stack* nil
  "A stack of prefix-to-URI mappings - bound by the parser.")

(defvar *namespace-counter* 0
  "Bound internally while unparsing to keep track of namespace
prefixes.")

(defvar *namespaces* nil
  "An alist of URI-to-prefix mappings - bound while unparsing.")

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-wbxml/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-wbxml
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
               
