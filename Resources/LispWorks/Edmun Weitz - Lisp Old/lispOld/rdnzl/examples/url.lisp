;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/examples/url.lisp,v 1.12 2010/05/18 10:55:37 edi Exp $

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :rdnzl-user)

(enable-rdnzl-syntax)

(import-types "System" "Net.WebClient" "Net.WebException")

(use-namespace "System.Net")

(defun download-url (url)
  (rdnzl-handler-case
   (let ((web-client (new "WebClient")))
      [GetString (new "System.Text.ASCIIEncoding")
                 [DownloadData web-client url]])
    ("WebException" (e)
      (warn "Ooops, probably a typo: ~A" [%Message e])
      nil)))

(disable-rdnzl-syntax)