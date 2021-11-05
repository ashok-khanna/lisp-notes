;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/libs.lisp,v 1.12 2011/02/11 19:56:05 edi Exp $

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

(in-package :starter-pack)

(defclass lib ()
  ((name :initarg :name
         :reader name
         :documentation "The \(ASDF) name of the library, a keyword.")
   (description :initarg :description
                :reader description
                :documentation "A short description of the
library, used for the tooltips in the graphical user
interaface.")
   (url :initarg :url
        :reader url
        :documentation "The download URL for the library.")
   (dependencies :initarg :dependencies
                 :reader dependencies
                 :documentation "A list of keywords denoting the
libraries this library depends on.")
   (checked :initform nil
            :accessor checked
            :documentation "Whether the user has checked this
library in the graphical user interface.")
   (required :initform nil
             :accessor required
             :documentation "Whether this library is required
\(directly or indirectly) by a library which was checked."))
  (:documentation "A LIB object is used internally to describe an
open source Lisp library."))

(defmethod initialize-instance :after ((lib lib) &rest initargs)
  (declare (ignore initargs))
  (let ((url (url lib)))
    ;; check if URL is correct and supported
    (unless (check-url url)
      (error "Can't work with URL ~S." url))
    ;; check if we can deal with the archive type of the library
    (unless (suffix (puri:uri-path (puri:parse-uri url)))
      (error "Suffix of file at URL ~S is not supported." (url lib))))
  ;; check if all dependencies can be found in *LIB-DESIGNATORS*
  (dolist (dependency (dependencies lib))
    (unless (find dependency *lib-designators* :key #'first)
      (error "Dependency ~S of library ~S not found."
             dependency (name lib)))))

(defun find-lib (name)
  "Finds and returns the LIB object with the name NAME, or NIL if
nothing is found."
  (find name *libs* :key #'name))

(defun checked-libs ()
  "Returns a list of all LIB objects which are checked."
  (loop for lib in *libs*
        when (checked lib)
        collect lib))

(defun compute-dependencies ()
  "Computes dependencies amongst all LIB objects.  Marks all LIBs
as `required' which are \(directly or indirectly) required by a
`checked' library."
  (dolist (lib *libs*)
    (setf (required lib) nil))
  ;; start with checked libs and those which always have to be there
  (let ((required-libs (nconc (loop for name in *required-libs*
                                    for lib = (find-lib name)
                                    when lib collect it)
                              (checked-libs))))
    (loop for lib = (pop required-libs)
          while lib
          do (dolist (dependency (dependencies lib))
               (pushnew (find-lib dependency) required-libs))
          unless (checked lib)
          do (setf (required lib) t))))

(defun check-url (string)
  "Checks whether STRING denotes a legal http\(s) URL."
  (ignore-errors
    (when-let (url (puri:parse-uri string))
      (member (puri:uri-scheme url) '(:http :https)))))

(defun read-config-file ()
  "Reads the file denoted by *CONFIG-FILE* \(if it exists) and adds
the library designators therein to *LIB-DESIGNATORS*."
  (when (probe-file *config-file*)
    (with-open-file (in *config-file*)
      (loop with eof = (list nil)
            for counter from 1
            for object = (read in nil eof)
            until (eq object eof)
            unless (and (listp object)
                        (>= (length object) 3)
                        (destructuring-bind (name description url &rest dependencies)
                            object
                          (and (keywordp name)
                               (stringp description)
                               (stringp url)
                               (check-url url)
                               (every #'keywordp dependencies))))
            do (capi:display-message "Syntax error in ~:R object of ~S - skipping rest."
                                     counter (namestring *config-file*))
               (return-from read-config-file)
            do (setq *lib-designators*
                     (cons object
                           (remove (first object) *lib-designators*
                                   :key #'first :test #'eq)))))))

;; yes, this is a pun
(defun read-my-libs ()
  "Reads the list *LIB-DESIGNATORS* and sets up the list *LIBS*
of corresponding LIB objects."
  (setq *libs*
        (sort
         (loop for (name description url . dependencies) in *lib-designators*
               collect (make-instance 'lib
                                      :name name
                                      :description description
                                      :url url
                                      :dependencies dependencies))
         #'string< :key #'name)))

(defun clsql-selected-p ()
  "Returns true if the user has selected the CLSQL library."
  (when-let (clsql (find-lib :clsql))
    (or (checked clsql) (required clsql))))