;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PLUGIN-EXAMPLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/plugin-example/init.lisp,v 1.8 2010/07/22 09:38:08 edi Exp $

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :plugin-example)

;; configure FM-PLUGIN-TOOLS for this particular plug-in
(setq *plugin-id* "LXmp"
      *plugin-name* "LispExample"
      #+:macosx #+:macosx
      *plugin-bundle-identifier* "weitz.de.LispExample"
      *plugin-help-text* "An example plug-in created with FM-PLUGIN-TOOLS."
      ;; see ASDF system definition
      *plugin-version* cl-user:*plugin-example-version*
      *company-name* "Edi Weitz"
      *copyright-message* "Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved."
      ;; we want a backtrace if something goes wrong
      *log-backtraces-p* t
      ;; the function defined in configuration.lisp
      *preferences-function* #'handle-configuration
      ;; enable idle messages so explicit garbage collection of older
      ;; generations happens
      *enable-idle-messages* t
      ;; defined in utils.lisp
      *init-function* #'init-plugin)
