;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-WIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-win/colors.lisp,v 1.5 2010/08/10 15:02:44 edi Exp $

;;; Copyright (c) 2008-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :lw-win)

(define-foreign-function (%get-sys-color "GetSysColor")
    ((index :int))
  :result-type :unsigned-long
  :documentation "Win32 function to query the system for
theme-specific colors.")

(define-foreign-function (%get-sys-color-brush "GetSysColorBrush")
    ((index :int))
  :result-type :pointer
  :documentation "Win32 function to query the system for \(handles
identifying) theme-specific brushes.")

(defun make-color-spec-from-sys-color (sys-color)
  "Converts a system color returned by a Win32 API call into a RGB
color specification usable by CAPI."
  (and sys-color
       (color:make-rgb (float (/ (logand sys-color #xff) #xff))
                       (float (/ (logand (ash sys-color -8) #xff) #xff))
                       (float (/ (ash sys-color -16) #xff)))))

(defun get-sys-color (index)
  "Returns a CAPI RGB color specification corresponding to the integer
identifier INDEX if the color is supported.  Returns NIL otherwise."
  (make-color-spec-from-sys-color
   ;; we need to call GetSysColorBrush first to check for the
   ;; null pointers (which could otherwise be interpreted as black)
   (and (not (null-pointer-p (%get-sys-color-brush index)))
        (%get-sys-color index))))
