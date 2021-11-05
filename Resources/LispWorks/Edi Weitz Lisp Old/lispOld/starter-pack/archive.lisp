;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/archive.lisp,v 1.12 2011/02/11 19:56:05 edi Exp $

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

(defun extract-tar-archive (pathname)
  "Extracts the tar archive denoted by PATHNAME into the library
folder."
  (archive:with-open-archive (archive pathname :direction :input)
    (archive:do-archive-entries (entry archive)
      (let ((name (archive:name entry)))
        (unless (fad:directory-pathname-p name)
          (let ((stream (archive:entry-stream entry))
                (target (merge-pathnames name (lib-dir))))
            (with-open-file (out (ensure-directories-exist target)
                                 :element-type 'octet
                                 :direction :output
                                 :if-exists :supersede)
              (fad:copy-stream stream out))))))))

(defun extract-gzipped-tar-archive (pathname)
  "Extracts the gzipped tar archive denoted by PATHNAME into the
library folder."
  (extract-tar-archive (gzip-stream:gunzip pathname (get-temp-file "tar"))))
              
(defun extract-zip-archive (pathname)
  "Extracts the ZIP archive denoted by PATHNAME into the library
folder."
  (zip:unzip pathname (ensure-directories-exist (lib-dir))
             :if-exists :supersede))
      
(defun extract-archive (pathname)
  "Extracts an archive \(tar, gzipped tar, or ZIP) denoted by
PATHNAME into the library folder."
  (let ((suffix (suffix (file-namestring pathname))))
    (funcall (cond ((string= suffix "zip") #'extract-zip-archive)
                   ((string= suffix "tar") #'extract-tar-archive)
                   ;; at this point we know from earlier checks that
                   ;; the suffix must be ".tar.gz" or ".tgz"
                   (t #'extract-gzipped-tar-archive))
             pathname)))
