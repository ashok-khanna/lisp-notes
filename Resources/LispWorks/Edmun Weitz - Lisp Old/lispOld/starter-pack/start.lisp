;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/start.lisp,v 1.19 2011/02/11 19:56:05 edi Exp $

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

;;; This file is part of the "Lisp Starter Pack" - see
;;; <http://weitz.de/starter-pack/> for details.

(in-package :cl-user)

;; set up some variables here so that they won't be overwritten in
;; LW-ADD-ONS's init file
(defvar *asdf-pathname* (make-pathname :name "asdf"
                                       :type nil
                                       :defaults *load-pathname*)
  "Where ASDF can be found.  This pathname should not have a type.")

(defvar *working-dir* (make-pathname :name nil
                                     :type nil
                                     :defaults *load-pathname*)
  "The directory containing this file \(at load time).")

(defvar *asdf-base-dirs* (list *working-dir*)
  "A list of directories where Lisp libraries are stored.")

;; set PATH environment variable so that SQLite DLL will be found
(setf (environment-variable "PATH")
      #+:win32
      (format nil "~A~@[;~A~]" (namestring *working-dir*)
              (environment-variable "PATH"))
      #-:win32
      (format nil "~A~@[:~A~]" (string-right-trim "/" (namestring *working-dir*))
              (environment-variable "PATH")))

;; slightly ugly hack to prevent the LW-ADD-ONS code from redefining
;; this variable
(defadvice ((setf environment-variable) :do-nothing :around)
    (new-value name))

;; let the init file of LW-ADD-ONS do the rest
(load (merge-pathnames ".lispworks"
                       (first
                        (directory
                         (merge-pathnames "lw-add-ons*" *working-dir*)))))

;; revert the hack from above
(delete-advice (setf environment-variable) :do-nothing)

#+:win32
;; to "fix" USER-HOMEDIR-PATHNAME
;; see <http://support.microsoft.com/default.aspx?scid=kb;en-us;101507>
(setf (environment-variable "HOMEPATH")
      (directory-namestring *working-dir*)
      (environment-variable "HOMEDRIVE")
      (format nil "~:@(~A:~)" (pathname-host *working-dir*)))

;; load private init file if the user has one
(when (probe-file "~/.lw-init.lisp")
  (load "~/.lw-init.lisp"))
