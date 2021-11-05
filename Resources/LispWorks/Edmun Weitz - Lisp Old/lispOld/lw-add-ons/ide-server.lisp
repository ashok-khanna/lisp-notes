;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/ide-server.lisp,v 1.11 2015/03/06 12:54:25 edi Exp $

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

;;; This code is from the LispWorks Knowledgebase - see
;;; <http://www.lispworks.com/kb/55af67dc408cab568025687f004b1442.html>

(in-package :lw-add-ons)

(win32:define-dde-server lispworks-ide-server ()
                         ()
                         (:service "LispWorks"))

(win32:define-dde-dispatch-topic editor
                                 :server lispworks-ide-server)

(win32:define-dde-server-function (open :topic editor)
                                  :execute
                                  ((filename string))
                                  (let ((path (probe-file filename)))
                                    (when path
                                      (ed path)
                                      t)))

(defun run-lispworks-ide-server-loop ()
  "Starts the DDE server and runs its message loop."
  (win32:start-dde-server 'lispworks-ide-server)
  (loop (mp:wait-processing-events 1)))

(defvar *lispworks-ide-server-process-info*
  '("DDE IDE Server" () run-lispworks-ide-server-loop))

;; Make the server run automatically when LispWorks starts.
(pushnew *lispworks-ide-server-process-info* mp:*initial-processes*
         :test 'equal :key 'car)
