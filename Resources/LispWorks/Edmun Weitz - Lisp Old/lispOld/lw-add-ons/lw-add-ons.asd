;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/lw-add-ons.asd,v 1.63 2015/08/09 15:18:49 edi Exp $

;;; Copyright (c) 2005-2015, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :cl-user)

(defpackage :lw-add-ons-asd
  (:use :cl :asdf))

(in-package :lw-add-ons-asd)

(unless (find-symbol "LIST-PANEL-RIGHT-CLICK-SELECTION-BEHAVIOR" :capi)
  (pushnew :no-right-click-selection-behavior *features*))

(when (find-symbol "*DONT-UNDO*" :editor)
  (pushnew :editor-has-dont-undo *features*))

(unless (find-symbol "WITH-BUFFER-LOCKED" :editor)
  (pushnew :editor-does-not-have-with-buffer-locked *features*))

(unless (find-symbol "I-FIND-PATTERN" :editor)
  (pushnew :editor-does-not-have-i-find-pattern *features*))

(unless (find-symbol "ABBREVIATED-COMPLETE-SYMBOL-COMMAND" :editor)
  (pushnew :editor-does-not-have-abbreviated-complete-symbol *features*))

(unless (find-symbol "GO-BACK-COMMAND" :editor)
  (pushnew :editor-does-not-have-go-back *features*))

(unless system::*auto-start-environment-p*
  (pushnew :console-image *features*))

(pushnew :lw-add-ons *features*)

#-:lispworks7
(require "hqn-web")

#+(and :win32 (not :console-image))
(require "dde")

(asdf:defsystem :lw-add-ons
  :version "0.10.3"
  :serial t
  :components ((:file "packages")
               (:file "specials")
               (:file "misc")
               (:file "documentation")
               #+(and :win32 (not :console-image)) (:file "ide-server")
               (:file "apropos")
               (:file "completions")
               (:file "systems")
               (:file "editor")
               (:file "commands"))
  :depends-on (:lw-doc))
