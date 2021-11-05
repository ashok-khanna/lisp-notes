;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/deliver.lisp,v 1.16 2010/07/22 09:38:05 edi Exp $

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

(in-package :cl-user)

(lw:load-all-patches)

;;; Change the values of the following special variables to match your
;;; settings.

;;; If you want to build the plug-in example that comes with
;;; FM-PLUGIN-TOOLS, you just need to adjust the first TWO values.

(defvar *asdf-location* #+:win32 "c:/home/lisp/asdf.lisp"
                        #+:macosx "/Users/karlpurtz/lisp/asdf.lisp"
  "Where ASDF can be found.")

(defvar *asdf-base-dirs* #+:win32 '("c:/home/lisp/")
                         #+:macosx '("/Users/karlpurtz/lisp/")
  "A list of directories \(note trailing slashes) which contain
directories that contain ASDF system definitions.

Example: If you have, say, c:/home/lisp/cl-ppcre/cl-ppcre.asd and
c:/home/lisp/tbnl/tbnl.asd, then \"c:/home/lisp/\" should be in this
list, and NOT \"c:/home/lisp/cl-ppcre/\".")

(defvar *asdf-system* :plugin-example
  "The ASDF system which contains the code for the plug-in.  It should
depend on FM-PLUGIN-TOOLS.")

(defvar *capi-required-p* #+:win32 t #+:macosx nil
  "Whether the plug-in needs CAPI.")

(defvar *mp-required-p* nil
  "Whether the plug-in needs MP.  This is only relevant if
*CAPI-REQUIRED-P* is NIL.")

(defvar *deliver-level* 0
  ;; level 0 is good for development - for deployment you most likely
  ;; want higher values for a much smaller DLL file
  "Delivery level for the delivered DLL.")

(defvar *start-function* 'lw:do-nothing
  "The start function of the delivered DLL.")

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

;; the following two functions are from LW-ADD-ONS
(defun walk-directory-for-asdf (dir)
  "Looks into the directory DIR and all subdirectories and adds all
directories which contain files of type \"asd\" to
ASDF:*CENTRAL-REGISTRY*."
  (dolist (dir-candidate (directory (lw:pathname-location dir)))
    (when (lw:file-directory-p dir-candidate)
      (walk-directory-for-asdf dir-candidate)
      (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
        (when (directory asd-candidate)
          (pushnew dir-candidate asdf:*central-registry* :test #'equal))))))
(compile 'walk-directory-for-asdf)

(defun update-asdf-central-registry ()
  "Loops through *ASDF-BASE-DIRS* recursively and adds all
directories containing system definitions to ASDF's central
registry."
  (dolist (base-dir *asdf-base-dirs*)
    (walk-directory-for-asdf base-dir)))
(compile 'update-asdf-central-registry)

;; tell ASDF about existing system definitions
(update-asdf-central-registry)

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
(asdf:oos 'asdf:load-op *asdf-system*)

(fm:set-product-name)

(defvar *versioninfo*
  `(:binary-version ,(logior (ash (or (first fm:*plugin-version*) 0) 48)
                             (ash (or (second fm:*plugin-version*) 0) 32)
                             (ash (or (third fm:*plugin-version*) 0) 16)
                             (or (fourth fm:*plugin-version*) 0))
    :version-string ,(fm:version-string)
    :company-name ,fm:*company-name*
    :product-name ,fm:*product-name*
    :file-description ,fm:*plugin-help-text*
    :legal-copyright ,fm:*copyright-message*))

;; check if plug-in ID is valid
(fm:check-plugin-id)

;; load necessary code for WRITE-MACOS-APPLICATION-BUNDLE
#+:macosx
(compile-file (example-file "configuration/macos-application-bundle")
              :output-file :temp
              :load t)

;; create shared library
(lw:deliver *start-function*
            ;; we assume this is called from the build script
            #+:win32
            (format nil "~A.fmx" (fourth sys:*line-arguments-list*))
            #+:macosx
            (write-macos-application-bundle
             (format nil "~A/~A.fmplugin"
                     (fifth sys:*line-arguments-list*)
                     (fourth sys:*line-arguments-list*))
             :template-bundle (make-pathname :name nil
                                             :type nil
                                             :version nil
                                             :defaults (merge-pathnames "Template.fmplugin/"
                                                                        *load-pathname*))
             :identifier fm:*plugin-bundle-identifier*
             :version (fm:version-string)             
             :executable-name (fourth sys:*line-arguments-list*)
             :document-types nil)
            *deliver-level*
            ;; we need a loadable bundle, not a Mach-O shared library
	    #+:macosx #+:macosx
	    :image-type :bundle
            :keep-symbols fm:*symbols-to-keep*
            :keep-lisp-reader t
            :keep-debug-mode (or fm:*log-backtraces-p* (< *deliver-level* 5))
            :versioninfo *versioninfo*
            :dll-exports '("FMExternCallProc")
            :interface (and *capi-required-p* :capi)
            :multiprocessing (or *capi-required-p* *mp-required-p*))))

