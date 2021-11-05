;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/specials.lisp,v 1.47 2011/02/11 19:56:05 edi Exp $

;;; Copyright (c) 2006-2011, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :starter-pack)

(defvar *lib-designators*
  '((:archive
     "Read and write tar and cpio archives."
     "http://www.method-combination.net/lisp/files/archive.tar.gz"
     :trivial-gray-streams)
    (:chunga
     "Portable chunked streams."
     "http://weitz.de/files/chunga.tar.gz"
     :trivial-gray-streams)
    (:cl-base64
     "Base64 decoding and encoding."
     "http://files.b9.com/cl-base64/cl-base64-latest.tar.gz")
    (:cl-fad
     "Portable pathname operations."
     "http://weitz.de/files/cl-fad.tar.gz")
    (:cl-interpol
     "String interpolation."
     "http://weitz.de/files/cl-interpol.tar.gz"
     :cl-unicode)
    (:cl-ppcre
     "Perl-compatible regular expressions."
     "http://weitz.de/files/cl-ppcre.tar.gz")
    (:cl-smtp
     "SMTP client functionality."
     "http://common-lisp.net/project/cl-smtp/cl-smtp.tar.gz")
    (:cl-unicode
     "A portable Unicode library."
     "http://weitz.de/files/cl-unicode.tar.gz"
     :cl-ppcre)
    (:cl-utilities 
     "A library of semi-standard utilities."
     "http://www.common-lisp.net/project/cl-utilities/cl-utilities-latest.tar.gz")
    (:cl-who
     "Markup language for HTML or XML."
     "http://weitz.de/files/cl-who.tar.gz")
    (:clsql
     "Talk to SQL databases."
     "http://files.b9.com/clsql/clsql-latest.tar.gz"
     :uffi)
    (:cxml
     "XML parser."
     ;; use CVS tarball for now
     "http://common-lisp.net/cgi-bin/viewcvs.cgi/cxml.tar.gz?root=cxml&view=tar"
     :puri :trivial-gray-streams)
    (:drakma
     "An HTTP client"
     "http://weitz.de/files/drakma.tar.gz"
     :chunga :cl-base64 :flexi-streams :puri)
    (:flexi-streams
     "Flexible bivalent streams."
     "http://weitz.de/files/flexi-streams.tar.gz"
     :trivial-gray-streams)
    (:gzip-stream
     "GZIP streams."
     "http://common-lisp.net/project/gzip-stream/files/gzip-stream_latest.tgz"
     :salza :flexi-streams)
    (:html-template
     "Template library for HTML."
     "http://weitz.de/files/html-template.tar.gz")
    (:hunchentoot
     "Web server."
     "http://weitz.de/files/hunchentoot.tar.gz"     
     :md5 :cl-base64 :rfc2388 :cl-ppcre :url-rewrite :chunga :flexi-streams :gzip-stream :cl-fad :trivial-backtrace)
    (:lw-add-ons
     "Enhancement to the LispWorks IDE."
     "http://weitz.de/files/lw-add-ons.tar.gz"
     :lw-doc)
    (:lw-doc
     "Permuted index for LispWorks documentation."
     "http://weitz.de/files/lw-doc.tar.gz"
     :cl-ppcre :cl-who)
    (:md5
     "The MD5 message-digest algorithm."
     "http://files.b9.com/md5/md5-1.8.5.tar.gz")
    (:puri
     "Parse and represent URIs."
     "http://files.b9.com/puri/puri-latest.tar.gz")
    (:rfc2388
     "Handles the multipart/form-data media type."
     "http://common-lisp.net/project/rfc2388/rfc2388_latest.tar.gz")
    (:salza
     "Compression library."
     "http://www.xach.com/lisp/salza/salza-0.7.2.tar.gz")
    (:split-sequence
     "Splits sequences."
     "http://ftp.linux.org.uk/pub/lisp/cclan/split-sequence.tar.gz")
    (:starter-pack
     "The source code for this program."
     "http://weitz.de/files/starter-pack.tar.gz"
     :archive :zip :gzip-stream :cl-fad :drakma :cl-ppcre)
    (:trivial-backtrace
     "Platform-independent backtraces."
     "http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.tar.gz")
    (:trivial-gray-streams
     "Gray streams portability library."
     "http://common-lisp.net/project/cl-plus-ssl/download/trivial-gray-streams.tar.gz")
    (:uffi
     "Portable access to C libraries."
     "http://files.b9.com/uffi/uffi-latest.tar.gz")
    (:url-rewrite
     "Rewrite HTML attributes."
     "http://weitz.de/files/url-rewrite.tar.gz")
    (:zip
     "Read and write ZIP files."
     ;; original address is <http://common-lisp.net/project/zip/zip.tgz>
     ;; see documentation
     "http://weitz.de/files/zip-cvs-2006-08-16.tar.gz"
     :salza)
    (:zpb-exif
     "Read EXIF data from digital images."
     "http://www.xach.com/lisp/zpb-exif.tgz"))
  "A list of open source Lisp libraries.  The first part of each entry
is the name of the library as a keyword, followed by a short
description, followed by an http URL to get the latest version,
followed by an optional list of requirements.")

(defvar *libs* nil
  "A list of LIB objects corresponding to the entries in
*LIB-DESIGNATORS*.")

(defvar *required-libs* '(:lw-add-ons)
  "A list of libraries which will always be downloaded.")

(defvar *temp-files* nil
  "A list of temporary files which should be deleted when the
image exits.")

(defvar *asdf-url* "http://common-lisp.net/project/asdf/asdf.lisp"
  "The URL to get the `asdf.lisp' file from.")

(defvar *sqlite-url* "http://www.sqlite.org/sqlitedll-3_5_4.zip"
  "The URL used to retrieve the SQLite3 DLL.")

(defvar *start-file-url* "http://weitz.de/starter-pack/start.lisp"
  "The URL for the `start.lisp' file.")

(defvar *lib-buttons* nil
  "Holds the \(CAPI) check buttons for the libraries.")

(defvar *status-pane* nil
  "Holds the status pane in the graphical user interface.")

;; the pathname of the (optional) config file
(define-symbol-macro *config-file*
  (merge-pathnames "config.lisp" (relative-directory)))

(defconstant +internet-settings+
  "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\\"
  "Windows registry path for Internet \(MSIE) settings.")

(defvar *http-proxy* nil
  "Proxy server for http - or NIL if none is used.")
(defvar *http-proxy-port* nil
  "Proxy port for http - or NIL for default value.")
(defvar *https-proxy* nil
  "Proxy server for https - or NIL if none is used.")
(defvar *https-proxy-port* nil
  "Proxy port for https - or NIL for default value.")
