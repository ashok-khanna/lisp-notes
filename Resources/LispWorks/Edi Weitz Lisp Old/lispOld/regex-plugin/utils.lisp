;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGEX-PLUGIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/regex-plugin/utils.lisp,v 1.8 2008/01/07 21:40:36 edi Exp $

;;; Copyright (c) 2006-2008, Dr. Jens Teich and Dr. Edmund Weitz.  All rights reserved.

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

(in-package :regex-plugin)

(defun read-config-values ()
  "Reads configuration values which should persist between
invocations of the plug-in from the Windows registry and sets the
corresponding Lisp variables."
  (multiple-value-bind (cache-regexes-p foundp)
      (plugin-preference "Configuration" "CacheRegexes")
    (when foundp
      (setq *cache-regexes-p* cache-regexes-p))))

(defun store-config-values ()
  "Writes configuration values which should persist between
invocations of the plug-in into the Windows registry."
  (setf (plugin-preference "Configuration" "CacheRegexes")
        *cache-regexes-p*))

(defun show-regex-cache-size ()
  "Returns a string \(suitable for the configuration dialog)
showing the occupancy of *REGEX-CACHE*."
  (format nil "  [Cache currently contains ~A entr~:@P.]"
          (hash-table-count *regex-cache*)))

(defun get-cached-regex (regex)
  "Returns a CL-PPCRE scanner corresponding to the regex string
REGEX and the current values of *CASE-INSENSITIVE-MODE*,
*MULTI-LINE-MODE*, *SINGLE-LINE-MODE*, and *EXTENDED-MODE*.
Tries to get the scanner from *REGEX-CACHE* if possible.  If the
scanner is not in the cache and *CACHE-REGEXES-P* is true, the
scanner is cached."
  (let ((args (list regex
                    :case-insensitive-mode *case-insensitive-mode*
                    :multi-line-mode *multi-line-mode*
                    :single-line-mode *single-line-mode*
                    :extended-mode *extended-mode*)))
    (multiple-value-bind (scanner scanner-found-p)
        (gethash args *regex-cache*)
      (unless scanner-found-p
        (setq scanner (apply #'ppcre:create-scanner args))
        (when *cache-regexes-p*
          (setf (gethash args *regex-cache*) scanner)))
      scanner)))

(defun modifier (char flags)
  "Returns a true value if FLAGS is a string \(and not NIL) and
the character CHAR appears in FLAGS, ignoring case."
  (and (stringp flags)
       (find char flags :test #'char-equal)))

(defmacro with-flags ((flags) &body body)
  "Executes BODY with *CASE-INSENSITIVE-MODE*,
*MULTI-LINE-MODE*, *SINGLE-LINE-MODE*, *EXTENDED-MODE*,
*APPLY-STYLE*, and *EVALUATEP* bound to values which correspond
to FLAGS, i.e. if FLAGS contains the character #\i, then
*CASE-INSENSITIVE-MODE* is bound to a true value, and so on."
  (rebinding (flags)
    `(let ((*case-insensitive-mode* (modifier #\i ,flags))
           (*multi-line-mode* (modifier #\m ,flags))
           (*single-line-mode* (modifier #\s ,flags))
           (*extended-mode* (modifier #\x ,flags))
           (*apply-style* (modifier #\c ,flags))
           (*evaluatep* (modifier #\e ,flags)))
       ,@body)))