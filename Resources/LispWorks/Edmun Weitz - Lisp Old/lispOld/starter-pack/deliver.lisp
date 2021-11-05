;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/deliver.lisp,v 1.15 2011/02/11 19:56:05 edi Exp $

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

(in-package :cl-user)

(lw:load-all-patches)

;;; Change the values of the following special variables to match your
;;; settings, if you also want to build an executable.

(defparameter *asdf-location* "c:/home/lisp/asdf.lisp"
  "Where ASDF can be found.")

(defparameter *asdf-system-locations* '("c:/home/lisp/")
  "A list of directories \(note trailing slashes) which contain
directories that contain ASDF system definitions.

Example: If you have, say, c:/home/lisp/cl-ppcre/cl-ppcre.asd and
c:/home/lisp/tbnl/tbnl.asd, then \"c:/home/lisp/\" should be in this
list, and NOT \"c:/home/lisp/cl-ppcre/\".  Also note that these
directories aren't scanned recursively.")

(defparameter *deliver-level* 5
  ;; level 0 is good for development - for deployment you most likely
  ;; want higher values for a much smaller DLL file
  "Delivery level for the delivered executable.")

;;; Usually, you shouldn't need to change anything below this point.

;; load ASDF, compile it if needed
#-:asdf
(let ((compilation-target (compile-file-pathname *asdf-location*)))
  (unless (probe-file compilation-target)
    (compile-file *asdf-location*))
  (handler-case
      (load compilation-target)
    ;; re-compile if old FASL version
    (conditions:fasl-error ()
      (load (compile-file *asdf-location*)))))

;; tell ASDF about existing system definitions
#+:asdf
(dolist (asdf-system-location *asdf-system-locations*)
  (dolist (dir-candidate (directory (merge-pathnames "*" asdf-system-location)))
    (when (lw:file-directory-p dir-candidate)
      (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
        (when (directory asd-candidate)
          (push dir-candidate asdf:*central-registry*))))))

#+:asdf
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  "Makes sure FASL files are re-compiled if they have the wrong
version."
  (handler-case
      (call-next-method o c)
    (conditions:fasl-error ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

;; load the code
(asdf:oos 'asdf:load-op :starter-pack)

(defparameter *versioninfo*
  `(:binary-version ,(logior (ash (or (first *starter-pack-version*) 0) 48)
                             (ash (or (second *starter-pack-version*) 0) 32)
                             (ash (or (third *starter-pack-version*) 0) 16)
                             (or (fourth *starter-pack-version*) 0))
    :version-string ,(starter-pack-version-string)
    :company-name "Edi Weitz"
    :product-name "Lisp Starter Pack"
    :file-description "A quick hack to download and install some Common Lisp open source libraries."
    :legal-copyright "Copyright (c) 2006-2011, Dr. Edmund Weitz.  All rights reserved."))

;; create EXE
(lw:deliver 'starter-pack:main
            (format nil "~A.exe" (fourth sys:*line-arguments-list*))
            *deliver-level*
            :versioninfo *versioninfo*
            :interface :capi
            ;; needed for config file
            :keep-lisp-reader t)

