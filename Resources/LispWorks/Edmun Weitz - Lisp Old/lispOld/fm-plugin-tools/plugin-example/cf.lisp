;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PLUGIN-EXAMPLE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/plugin-example/cf.lisp,v 1.5 2010/07/22 09:38:08 edi Exp $

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

;;; The contents of this file are actually not very interesting.  We
;;; do some FLI gymnastics do get at OS X locale information.

(in-package :plugin-example)

(defconstant +k-cf-string-encoding-iso-latin-1+ #x0201
  ;; see <http://developer.apple.com/documentation/CoreFoundation/Reference/CFStringRef/Reference/reference.html#//apple_ref/c/tdef/CFStringBuiltInEncodings>
  "Constant denoting the built-in string enconding for ISO-8859-1.")

(define-foreign-function (cf-release "CFRelease")
    ((thing (:pointer :void)))
  :documentation "Releases the Core Foundation object pointed to by THING."
  :result-type :void)

(defmacro with-cf-object ((var form) &body body)
  "Evaluates FORM to create a Core Foundation object which is bound to
VAR while BODY is executed.  The object is guaranteed to be freed with
CFRelease."
  `(let (,var)
     (unwind-protect
         (progn
           (setq ,var ,form)
           ,@body)
       (when ,var
         (ignore-errors (cf-release ,var))))))

(define-foreign-function (cf-string-create-with-c-string "CFStringCreateWithCString")
    ((allocator (:pointer :void))
     (string (:reference-pass :ef-mb-string))
     (encoding (:unsigned :int)))
  :documentation "Accepts a Lisp \(multi-byte) string and returns a
corresponding CFString object \(which we'll have to free with
CFRelease after we're done).  Uses the string encoding ENCODING."
  :lambda-list (string &key
                       ;; default is ISO-8859-1
                       (encoding +k-cf-string-encoding-iso-latin-1+)
                       ;; use the current default allocator
                       (allocator *null-pointer*))
  :result-type (:pointer :void))

(defmacro with-cf-string ((var string) &body body)
  "Executes BODY with VAR bound to a CFString corresponding to the
\(multi-byte) Lisp string STRING.  The CFString is guaranteed to be
freed with CFRelease."
  `(with-cf-object (,var (cf-string-create-with-c-string ,string))
     ,@body))
  
(define-foreign-function (get-bundle% "CFBundleGetBundleWithIdentifier")
    ((cf-string (:pointer :void)))
  :documentation "Locates a bundle given its identifier.  Can return
NULL if the bundle wasn't found as it is not loaded automatically."
  :result-type (:pointer :void))

(defun get-bundle (identifier)
  "Locates a bundle given its identifier.  Can return NIL if the
bundle wasn't found."
  (with-cf-string (cf-string identifier)
    (let ((bundle (get-bundle% cf-string)))
      (and (not (null-pointer-p bundle)) bundle))))

(defun try-to-locate-bundle ()
  "Tries to locate the Carbon or, failing that, the Cocoa bundle.
Signals an error if both can't be located."
  (or (get-bundle "com.apple.Carbon")
      (get-bundle "con.apple.Cocoa")
      (error "Couldn't locate the Carbon and Cocoa bundles.")))

(define-foreign-function (get-data-pointer% "CFBundleGetDataPointerForName")
    ((bundle (:pointer :void))
     (cf-string (:pointer :void)))
  :documentation "Returns a data pointer to a symbol of the given name
in the specified bundle.  The pointer must be dereferenced before it
is used."
  :result-type (:pointer (:pointer :void)))

(defun get-data-pointer (bundle name)
  "Returns a data pointer for the symbol named by the Lisp string NAME."
  (with-cf-string (cf-string name)
    (dereference (get-data-pointer% bundle cf-string))))

(define-foreign-function (cf-string-get-c-string "CFStringGetCString")
    ((cf-string (:pointer :void))
     (c-string (:reference-return (:ef-mb-string :limit 256)))
     (size (:signed :int))
     (encoding (:unsigned :int)))
  :documentation "Copies the character contents of a CFString to a
Lisp string via an intermediate C string.  Only works for strings with
less than 256 characters.  The first return value is a boolean
denoting success of the operation, the second return value is the
actual string."
  :lambda-list (cf-string &key (encoding +k-cf-string-encoding-iso-latin-1+)
                          &aux (size 256) c-string)
  :result-type :boolean)

(defconstant +k-cf-locale-decimal-separator+ "kCFLocaleDecimalSeparator"
  "Symbol name for the decimal separator locale key.")

(define-foreign-function (cf-locale-copy-current "CFLocaleCopyCurrent")
    ()
  :documentation "Returns a copy of the logical locale for the current
user.  Result must eventually be freed with CFRelease."
  :result-type (:pointer :void))

(define-foreign-function (cf-locale-get-value "CFLocaleGetValue")
    ((locale (:pointer :void))
     (cf-string (:pointer :void)))
  :documentation "Returns the corresponding value for a given key in
the specified locale."
  :result-type (:pointer :void))

(defun get-locale-value-as-string (symbol-name)
  "Tries to find the locale value denoted by the key SYMBOL-NAME \(a
Lisp string) and returns it as a Lisp string.  Returns NIL if
unsuccessful."
  (let ((key (get-data-pointer (try-to-locate-bundle) symbol-name)))
    (with-cf-object (locale (cf-locale-copy-current))
      (multiple-value-bind (successp result)
          (cf-string-get-c-string (cf-locale-get-value locale key))
        (and successp result)))))
