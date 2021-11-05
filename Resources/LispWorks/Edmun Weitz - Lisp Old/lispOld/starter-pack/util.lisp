;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/util.lisp,v 1.26 2011/02/11 19:56:05 edi Exp $

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

;; the LispWorks TCP/IP interface - we require it explictely for the
;; sake of the delivered executable
(require "comm")

(defun development-image-p ()
  "Whether we're in the IDE or in a delivered executable."
  (fboundp 'compile-file))

(defun relative-directory (&rest components)
  "Results in a pathname describing a directory relative to the
directory of the Lisp source files \(while you're developing in the
IDE) or the executable \(after delivery)."
  (let* ((app-truename (cond ((development-image-p)
                              (load-time-value *load-truename*))
                             (t (lw:lisp-image-name))))
         (load-directory (pathname-directory app-truename)))
    (merge-pathnames (make-pathname :directory (append load-directory components)
                                    :name :unspecific
                                    :type :unspecific
                                    :version :unspecific)
                     app-truename)))

(defun ends-with-p (string suffix &key (test #'char-equal))
  "Returns true if STRING ends with the string SUFFIX.
Individual characters are compared with TEST."
  (let ((mismatch (mismatch string suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length string) (length suffix))))))

(defun suffix (string)
  "If STRING ends with one of the four accepted suffixes, this
suffix is returned, otherwise NIL."
  (loop for candidate in '(".tar.gz" ".tgz" ".zip" ".tar")
        when (ends-with-p string candidate)
        do (return (subseq candidate 1))))

;; a workaround which isn't needed for 5.1 anymore
#+(or :lispworks4.4 :lispworks5.0)
(defun get-folder-path (what)
  "Like SYS:GET-FOLDER-PATH but returns a correct pathname."
  ;; we use PROBE-FILE to make this a directory
  (probe-file (string-append (sys:get-folder-path what))))

(defun user-agent-string ()
  "Returns a user agent string which will be used by Drakma."
  (format nil "Lisp Starter Pack ~A" (starter-pack-version-string)))

(defun version< (string1 string2)
  "Checks if version denoted by STRING1 is older than version denoted
by STRING2.  It is assumed that both strings look like \"a.b.d\" where
a, b, and c are non-negative decimal numbers."
  (ppcre:register-groups-bind ((#'parse-integer v1-1 v1-2 v1-3))
      ("^(\\d+)\\.(\\d+)\\.(\\d+)$" string1)
    (ppcre:register-groups-bind ((#'parse-integer v2-1 v2-2 v2-3))
        ("^(\\d+)\\.(\\d+)\\.(\\d+)$" string2)
      (or (< v1-1 v2-1)
          (and (= v1-1 v2-1)
               (or (< v1-2 v2-2)
                   (and (= v1-2 v2-2)
                        (< v1-3 v2-3))))))))

(defun lib-dir ()
  "Returns a pathname denoting the directory where the libraries
are to be installed."
  ;; we use a little trick to appease the ZIP library
  (let ((defaults
         (merge-pathnames #+:win32 "Lisp Libraries/"
                          #-:win32 "Lisp-Libraries/"
                          #-(or :lispworks4.4 :lispworks5.0) (sys:get-folder-path :my-documents)
                          #+(or :lispworks4.4 :lispworks5.0) (get-folder-path :my-documents))))
    (make-pathname :name nil :type nil :defaults defaults)))

(defmacro with-open-temp-file ((stream-var suffix) &body body)
  "Creates a new temporary file with the suffix SUFFIX which will
be deleted when the image exits and returns the pathname of this
file.  If BODY is provided, the file is opened for writing and
BODY is executed with STREAM-VAR bound to the corresponding
output stream."
  (with-unique-names (temp-file)
    `(let ((,temp-file (make-temp-file nil ,suffix)))
       (push ,temp-file *temp-files*)
       ,@(and body
              `((with-open-file (,stream-var ,temp-file
                                             :direction :output
                                             :if-exists :supersede
                                             :element-type 'octet)
                 ,@body)))
       ,temp-file)))

(defun get-temp-file (suffix)
  "Uses WITH-OPEN-TEMP-FILE to return a new temporary file with
the suffix SUFFIX."
  (with-open-temp-file (out suffix)))

;; make sure temp files are deleted when the image exits
(define-action "When quitting image" "Delete temp files"
  (lambda ()
    (loop for file in *temp-files*
          do (ignore-errors (delete-file file)))))

(defun proxy-enabled-p ()
  "Looks into the Windows registry and returns a true value if a proxy
server is used."
  ;; note: only "static" setting is checked - see
  ;; <http://support.microsoft.com/?id=226473>
  #+:win32
  (not (zerop (win32:query-registry-value +internet-settings+ "ProxyEnable"))))

(defun set-proxy-info ()
  "Looks into the Windows registry and sets values like *HTTP-PROXY*
accordingly if PROXY-ENABLED-P returns a true value."
  ;; note: only "static" setting is checked - see
  ;; <http://support.microsoft.com/?id=226473>
  (setq *http-proxy* nil
        *http-proxy-port* nil
        *https-proxy* nil
        *https-proxy-port* nil)
  #+:win32
  (when (proxy-enabled-p)
    (let ((proxy-string (win32:query-registry-value +internet-settings+ "ProxyServer")))
      (dolist (proxy-entry (ppcre:split "[;\\s]+" proxy-string))
        (ppcre:register-groups-bind (protocol server (#'parse-integer port))
            ;; regex based on MSDN entry for WINHTTP_PROXY_INFO
            ("(?i)^(?:([a-z]+)=)?(?:[a-z]+://)?([a-z.]+)(?::(\\d+))?$" proxy-entry)
          (cond ((null protocol)
                 (setq *http-proxy* server
                       *http-proxy-port* (or port 80)
                       *https-proxy* *http-proxy*
                       *https-proxy-port* *http-proxy-port*)
                 (return))
                ((string-equal protocol "http")
                 (setq *http-proxy* server
                       *http-proxy-port* (or port 80)))
                ((string-equal protocol "https")
                 (setq *https-proxy* server
                       ;; 443 - is this the correct default value?
                       *https-proxy-port* (or port 443)))))))
    (when (or *http-proxy* *https-proxy*)
      (display-message "Proxy settings used:~:[~*~; http=~:*~A:~A~]~:[~;; https=~:*~A:~A~]"
                       *http-proxy* *http-proxy-port*
                       *https-proxy* *https-proxy-port*))))

(defun get-proxy (uri)
  "Returns a proxy value suitable for DRAKMA:HTTP-REQUEST based on the
URI URI and settings like *HTTP-PROXY*."
  (case (puri:uri-scheme (puri:parse-uri uri))
    (:http (and *http-proxy* (list *http-proxy* *http-proxy-port*)))
    (:https (and *https-proxy* (list *https-proxy* *https-proxy-port*)))))
