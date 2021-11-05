;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/util.lisp,v 1.5 2013/01/11 17:14:21 edi Exp $

;;; Copyright (c) 2007-2013, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :odd-streams)

(deftype octet ()
  "A shortcut for \(UNSIGNED-BYTE 8)."
  '(unsigned-byte 8))

#+:lispworks
(defmacro with-accessors (slot-entries instance &body body)
  "For LispWorks, we prefer SLOT-VALUE over accessors for better
performance."
  `(with-slots ,(mapcar #'car slot-entries)
       ,instance
     ,@body))

#+:lispworks
(defun sans (plist &rest keys)
  "Returns PLIST with keyword arguments from KEYS removed."
  (sys::remove-properties plist keys))

#-:lispworks
(defun sans (plist &rest keys)
  "Returns PLIST with keyword arguments from KEYS removed."
  ;; stolen from Usenet posting <3247672165664225@naggum.no> by Erik
  ;; Naggum
  (let ((sans ()))
    (loop
      (let ((tail (nth-value 2 (get-properties plist keys))))
        ;; this is how it ends
        (unless tail
          (return (nreconc sans plist)))
        ;; copy all the unmatched keys
        (loop until (eq plist tail) do
              (push (pop plist) sans)
              (push (pop plist) sans))
        ;; skip the matched key
        (setq plist (cddr plist))))))
