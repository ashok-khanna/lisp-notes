;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGEX-PLUGIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/regex-plugin/init.lisp,v 1.14 2008/01/07 20:33:09 edi Exp $

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

;; configure FM-PLUGIN-TOOLS for this particular plug-in
(setq *plugin-id* "RegP"
      *plugin-name* "RegexPlugIn"      
      #+:macosx #+:macosx
      *plugin-bundle-identifier* "jensteich.de.RegexPlugIn"
      *plugin-help-text* "A plug-in that provides the ability to work with regular expressions."
      ;; see ASDF system definition
      *plugin-version* cl-user:*regex-plugin-version*
      *company-name* "Jens Teich"
      *copyright-message* "Copyright (c) 2006-2008, Dr. Jens Teich and Dr. Edmund Weitz.  All rights reserved."
      *log-backtraces-p* nil
      ;; the function defined in configuration.lisp
      *preferences-function* #'handle-configuration
      ;; enable idle messages so explicit garbage collection of older
      ;; generations happens
      *enable-idle-messages* t
      ;; defined in utils.lisp - reads configuration values from
      ;; registry when the plug-in is initialized
      *init-function* #'read-config-values)