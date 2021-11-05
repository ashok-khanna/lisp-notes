;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/systems.lisp,v 1.21 2015/05/29 18:23:24 edi Exp $

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

(in-package :lw-add-ons)

(defun list-all-systems-known-to-asdf ()
  "Returns a list of all systems ASDF knows already."
  (loop for name being the hash-keys of asdf::*defined-systems*
        collect name))

(defun list-all-systems-in-central-registry ()
  "Returns a list of all systems in ASDF's central registry."
  (mapcar #'pathname-name
          (delete-duplicates
           (loop for dir in asdf:*central-registry*
                 for defaults = (eval dir)
                 when defaults
                 nconc (mapcar #'file-namestring
                               (directory
                                (make-pathname :defaults defaults
                                               :version :newest
                                               :type "asd"
                                               :name :wild
                                               :case :local))))
           :test #'string=)))

(defun list-asdf-systems ()
  "Returns the systems in ASDF's central registry and those which ASDF
already knows."
  (nunion (list-all-systems-known-to-asdf)
          (list-all-systems-in-central-registry)
          :test #'string=))

(defun pathname-name* (name)
  "First \(using ASDF) converts NAME to a string if it isn't one
already, then treats the resulting string as a pathname
designator and returns its name component."
  (pathname-name (asdf::coerce-name name)))

(defun component-foreign-dependencies (operation component)
  "The set difference of ASDF::COMPONENT-DEPENDS-ON and
ASDF::COMPONENT-SELF-DEPENDENCIES."
  (remove-if (lambda (dep)
               (member (asdf:component-name component) (cdr dep)
                       :test #'string=))
             (asdf:component-depends-on operation component)))

(defun translate-dep (dep)
  "Translate an ASDF dependency into a Common Defsystem
requirement."
  (ecase (first dep)
    (asdf:compile-op
     `(:compile ,@(mapcar #'pathname-name* (rest dep))))
    (asdf:load-op
     `(:load ,@(mapcar #'pathname-name* (rest dep))))))

(defun translate-deps (deps)
  "Translate a list of ASDF dependencies into a list of Common
Defsystem requirement."
  (loop for dep in deps
        collect (translate-dep dep)))

(defun make-unique-module-name (name parent-names)
  "Tries to create a `unique' module name from a list of parent
name strings and the name of an internal module itself."
  ;; note that we use "->" which we hope won't occur as the name of
  ;; some `real' ASDF system
  (intern (format nil "~{~A->~}~A"
                  (mapcar #'string-upcase parent-names)
                  (string-upcase name))
          :cl-user))

(let ((load-op (load-time-value
                 (make-instance 'asdf:load-op)))
      (compile-op (load-time-value
                    (make-instance 'asdf:compile-op))))
  (defun translate-module (module &optional parent-names)
    "Translates the ASDF module MODULE into a Common Defsystem
system definition.  If the module is not a `stand-alone' system
with its own .asd file then PARENT-NAMES is the list of the names
of its parent systems.  Returns the name of the module."
    ;; set to 0 temporarily as we'll have a lot of calls to INTERN
    (let ((*symbol-alloc-gen-num* 0)
          (module-name (asdf:component-name module))
          (module-pathname (asdf:component-pathname module))
          members rules substitutions global-deps)
      (labels ((to-symbol (name &key symbol no-subs)
                 "Converts the string NAME into a symbol in the
CL-USER package after upcasing it.  Registers this conversion in
the SUBSTITUTIONS alist unless NO-SUBS if true.  If SYMBOL is not
NIL take this argument as the resulting symbol, i.e. no
conversion, just registration."
                 (let ((symbol (or symbol
                                   (intern (string-upcase name) :cl-user))))
                   (unless no-subs
                     (push (cons name symbol) substitutions))
                   symbol))
               (resolve-global-deps (translated-deps)
                 "Accepts a list of dependencies \(requirements)
in Common Defsystem format and registers the involved components
as members of type :SYSTEM.  Returns its original argument."
                 (dolist (translated-dep translated-deps)
                   (dolist (candidate (rest translated-dep))
                     ;; make sure each members occurs only once
                     (unless (find candidate global-deps :test #'equal)
                       (push candidate global-deps)
                       (push `(,(to-symbol candidate) :type :system) members))))
                 translated-deps))
        (unless parent-names
          ;; if this is a "top-level" system record its "external"
          ;; dependencies as well (if there are any) - don't do this
          ;; for "internal" modules as they may depend on files in the
          ;; containing system which can't be expressed in Common
          ;; Defsystem
          (when-let (load-deps (component-foreign-dependencies load-op module))
            (push `(:in-order-to :load :all
                                 (:requires ,@(resolve-global-deps
                                               (translate-deps load-deps))))
                  rules))
          (when-let (compile-deps (component-foreign-dependencies compile-op module))
            (push `(:in-order-to :compile :all
                                 (:requires ,@(resolve-global-deps
                                               (translate-deps compile-deps))))
                  rules)))
        ;; loop through all components of the system
        (dolist (component (asdf:module-components module))
          (let* ((input-files (asdf:input-files compile-op component))
                 (input-file (first input-files))
                 (component-name (asdf:component-name component)))
            (when (cdr input-files)
              (error "More than one input file for component ~S." component-name))
            ;; first the requirement - note that we don't translate
            ;; the name here (as in NAME-TO-USE below)
            (when-let (load-deps (asdf::component-depends-on load-op component))
              (push `(:in-order-to :load (,component-name)
                                   (:requires ,@(translate-deps load-deps)))
                    rules))
            (when-let (compile-deps (asdf::component-depends-on compile-op component))
              (push `(:in-order-to :compile (,component-name)
                                   (:requires ,@(translate-deps compile-deps)))
                    rules))
            (etypecase component
              (asdf:system
               ;; an external system: just list it
               (push `(,(to-symbol component-name) :type :system) members))
              (asdf:module
               ;; a module: list it but also create it as a Common
               ;; Defsystem system - this ain't really correct as a
               ;; module isn't a `stand-alone' system but I see no
               ;; better way to do it as LW can't do nested `modules'
               (let ((child-name
                       (translate-module component
                                         (append parent-names (list module-name)))))
                 (push `(,(to-symbol component-name :symbol child-name) :type :system)
                       members)))
              ((or asdf:c-source-file asdf:cl-source-file)
               ;; a file: the tricky part is to get the name right
               (let* ((real-file-name (enough-namestring input-file module-pathname))
                      (file-type (or (pathname-type real-file-name)
                                     (asdf:source-file-type component module)))
                      ;; use the Common Defsystem file types if possible
                      (type (cond ((string-equal file-type "lisp")
                                   :lisp-file)
                                  ((string-equal file-type "lsp")
                                   :lsp-file)
                                  ((string-equal file-type "c")
                                   :c-file) 
                                  (t nil)))
                      ;; compute pathname of file from component name
                      ;; like Common Defsystem would do it
                      (path-computed-from-name (merge-pathnames
                                                (merge-pathnames (string component-name)
                                                                 (cond (type
                                                                        (make-pathname :type file-type))
                                                                       (t (make-pathname))))
                                                module-pathname))
                      ;; compute pathname of file from REAL-FILE-NAME
                      ;; like Common Defsystem would do it
                      (path-computed-from-file-name (merge-pathnames real-file-name
                                                                     module-pathname))
                      ;; decide which name to use for the component
                      ;; based on some value of `elegance' - we want
                      ;; it short if possible
                      (name-to-use (cond ((equal input-file path-computed-from-name)
                                          component-name)
                                         ((equal input-file path-computed-from-file-name)
                                          (namestring real-file-name))
                                         (t (namestring input-file)))))
                 ;; if we couldn't use the component name itself we
                 ;; have to register this conversion
                 (unless (equal component-name name-to-use)
                   (push (cons component-name (pathname-name* name-to-use))
                         substitutions))
                 ;; finally list it as a member
                 (push `(,name-to-use :type ,(or type :lisp-file)) members))))))
        (let ((module-name (cond (parent-names
                                  ;; if this module has parents then
                                  ;; construct an artifical name that
                                  ;; shows the heritage and tries to
                                  ;; make the module unique
                                  (make-unique-module-name module-name
                                                           parent-names))
                                 (t
                                  ;; otherwise just convert to symbol
                                  ;; without registering
                                  (to-symbol module-name :no-subs t)))))
          (eval
           `(defsystem ,module-name
                (:default-pathname ,module-pathname)
              :members ,(nreverse members)
              ;; now finally the substitutions
              :rules ,(nsublis substitutions (nreverse rules)
                               :test #'equal)))
          ;; may be useful for large systems...
          (gc-if-needed)
          module-name)))))

#-:lispworks7
(defadvice (asdf::parse-component-form translate :around
                                       :documentation "Whenever
an ASDF system is parsed we translate it to a Common Defsystem
system definition on the fly.")
    (parent options)
  (let ((candidate (call-next-advice parent options)))
    (when (and *translate-asdf-systems*
               (typep candidate 'asdf:system))
      (ignore-errors*
        (translate-module candidate)))
    candidate))

#-:lispworks7
;; translate the systems that have already been loaded
(dolist (sys-name '(:cl-ppcre :cl-who :lw-doc :lw-add-ons))
  (translate-module (asdf:find-system sys-name)))
