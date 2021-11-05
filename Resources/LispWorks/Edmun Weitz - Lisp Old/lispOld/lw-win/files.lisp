;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-WIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-win/files.lisp,v 1.8 2010/08/10 16:55:42 edi Exp $

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

;; there's also WIN32:CREATE-FILE serving a similar purpose, but it's
;; not documented and so it seems safer to use our own
(define-foreign-function (%create-file "CreateFile" :dbcs)
    ((file-name w:lpctstr)
     (desired-access dword)
     (share-mode dword)
     (security-attributes :pointer)
     (creation-disposition dword)
     (flags-and-attributes dword)
     (template-file handle))
  :lambda-list (file-name &key
                          (desired-access (logior +generic-read+ +generic-write+))
                          (share-mode 0)
                          (security-attributes *null-pointer*)
                          (creation-disposition +open-existing+)
                          (flags-and-attributes +file-attribute-normal+)
                          (template-file *null-pointer*))
  :result-type handle)

(defun create-file (pathspec &rest other-args)
  "A Lisp wrapper for the Win32 API function `CreateFile' which takes
care of error checking and translating the Lisp pathname designator
PATHSPEC into a string understood by Win32 but otherwise just calls
the underlying function.  See FLI definition above and MSDN
documentation for the meaning of OTHER-ARGS.

It is recommended to use WITH-OPEN-FILE-HANDLE instead of this
function."
  ;; the call to CURRENT-PATHNAME is to make sure we don't end up with
  ;; "incomplete" strings \(e.g. without drive letters) fed to Win32
  (let ((handle (apply '%create-file (namestring (current-pathname pathspec)) other-args)))
    (when (pointer-eq handle +invalid-handle-value+)
      (signal-last-error))
    ;; see CreateFile documentation
    (let ((error-code (%get-last-error)))
      (unless (zerop error-code)
        (signal-last-error :error-code error-code :warnp t)))
    handle))

(define-foreign-function (%close-handle "CloseHandle")
    ((object handle))
  :result-type bool)

(defun close-handle (handle &optional errorp)
  "A wrapper for the Win32 API function `CloseHandle' which takes care
of error checking.  Usually only warns if an error occurs, but will
signal an error if ERRORP is true."
  (unless (%close-handle handle)
    (signal-last-error :warnp (not errorp) :format "While closing handle: ~A")))

(defmacro with-open-file-handle ((handle pathspec &rest other-args) &body body)
  "Binds HANDLE to the results of applying CREATE-FILE to PATHSPEC and
OTHER-ARGS during the execution of BODY.  Makes sure that the handle
returned by CREATE-FILE is closed using CLOSE-HANDLE at the end."
  `(let (,handle)
     (unwind-protect
         (progn
           (setq ,handle (create-file ,pathspec ,@other-args))
           ,@body)
       (when ,handle
         (close-handle ,handle)))))

(define-foreign-function (%create-file-mapping "CreateFileMapping" :dbcs)
    ((file-handle handle)
     (security-attributes :pointer)
     (protect dword)
     (maximum-size-high dword)
     (maximum-size-low dword)
     (name :pointer))
  :lambda-list (file-handle &key
                            (security-attributes *null-pointer*)
                            (protect +page-readwrite+)
                            (maximum-size-high 0)
                            (maximum-size-low 0)
                            (name *null-pointer*))
  :result-type handle)

(defun create-file-mapping (file-handle &rest other-args)
  "A Lisp wrapper for the Win32 API function `CreateFileMapping' which
takes care of error checking but otherwise just calls the underlying
function.  FILE-HANDLER should be an object as returned by
CREATE-FILE.  See FLI definition above and MSDN documentation for the
meaning of OTHER-ARGS.

It is recommended to use WITH-FILE-MAPPING instead of this function."
  (let ((object (with-error-check () (apply '%create-file-mapping file-handle other-args))))
    ;; see CreateFileMapping documentation
    (let ((error-code (%get-last-error)))
      (unless (zerop error-code)
        (signal-last-error :error-code error-code :warnp t)))
    object))

(defmacro with-file-mapping ((object file-handle &rest other-args) &body body)
  "Binds OBJECT to the results of applying CREATE-FILE-MAPPING to
FILE-HANDLE and OTHER-ARGS during the execution of BODY.  Makes sure
that the object returned by CREATE-FILE-MAPPING is cleaned up using
CLOSE-HANDLE at the end."
  `(let (,object)
     (unwind-protect
         (progn
           (setq ,object (create-file-mapping ,file-handle ,@other-args))
           ,@body)
       (when ,object
         (close-handle ,object)))))

(define-foreign-function (%map-view-of-file "MapViewOfFile")
    ((file-mapping-object handle)
     (desired-access dword)
     (file-offset-high dword)
     (file-offset-low dword)
     (number-of-bytes-to-map :unsigned-long))
  :lambda-list (file-mapping-object file-offset-high file-offset-low
                                    &key
                                    (number-of-bytes-to-map 0)
                                    (desired-access +file-map-write+))
  :result-type :pointer)

(defun map-view-of-file (file-mapping-object &key
                                             (file-offset 0)
                                             (number-of-octets 0)
                                             (desired-access +file-map-write+))
  "A Lisp wrapper for the Win32 API function `MapViewOfFile' which
takes care of error checking and argument translation but otherwise
just calls the underlying function.  FILE-MAPPING-OBJECT should be an
object as returned by CREATE-FILE-MAPPING.  FILE-OFFSET is the offset
\(in octets) into the file and which values are legal for this
argument depends on the system's granularity.  NUMBER-OF-OCTETS is the
number of octets mapped into memory.  For DESIRED-ACCESS see the MSDN
documentation.

It is recommended to use WITH-MAPPED-VIEW instead of this function."
  (with-error-check ()
    (%map-view-of-file file-mapping-object
                       (ash file-offset -32)
                       (logand file-offset #xffffffff)
                       :number-of-bytes-to-map number-of-octets
                       :desired-access desired-access)))

(define-foreign-function (%unmap-view-of-file "UnmapViewOfFile")
    ((base-address :pointer))
  :result-type bool)

(defun unmap-view-of-file (base-address &optional errorp)
  "A wrapper for the Win32 API function `UnMapViewOfFile' which takes
care of error checking.  Usually only warns if an error occurs, but
will signal an error if ERRORP is true."
  (unless (%unmap-view-of-file base-address)
    (signal-last-error :warnp (not errorp) :format "While unmapping view of file: ~A")))

(defmacro with-mapped-view ((pointer file-mapping-object
                                     &rest rest
                                     &key
                                     (file-offset 0)
                                     (number-of-octets 0)
                                     (desired-access '+file-map-write+))
                            &body body)
  "Binds POINTER to the results of applying MAP-VIEW-OF-FILE to
FILE-MAPPING-OBJECT and REST during the execution of BODY.  Makes sure
that the pointer returned by MAP-VIEW-OF-FILE is cleaned up using
UNMAP-VIEW-OF-FILE at the end."
  (declare (ignore file-offset number-of-octets desired-access))
  `(let (,pointer)
     (unwind-protect
         (progn
           (setq ,pointer (map-view-of-file ,file-mapping-object ,@rest))
           ,@body)
       (when ,pointer
         (unmap-view-of-file ,pointer)))))

(defmacro with-mapped-file ((pointer pathspec &key (file-offset 0) (number-of-octets 0))
                            &body body)
  "Executes BODY with NUMBER-OF-OCTETS octecs of the contents of the
file denoted by the pathname designator PATHSPEC beginning at octet
position FILE-OFFSET mapped into RAM.  The contents are accessible
beginning at the position denoted by the FLI pointer POINTER.

Note that not all values for FILE-OFFSET are legal.  See the MSDN
documentation for details."
  (with-unique-names (file-handle mapping-object)
    `(with-open-file-handle (,file-handle ,pathspec)
       (with-file-mapping (,mapping-object ,file-handle)
         (with-mapped-view (,pointer ,mapping-object
                                     :file-offset ,file-offset
                                     :number-of-octets ,number-of-octets)
           ,@body)))))
