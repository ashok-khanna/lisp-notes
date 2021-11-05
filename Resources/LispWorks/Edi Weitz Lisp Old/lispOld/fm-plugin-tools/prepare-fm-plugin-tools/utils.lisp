;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PREPARE-FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/prepare-fm-plugin-tools/utils.lisp,v 1.12 2010/07/22 09:38:10 edi Exp $

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

(in-package :prepare-fm-plugin-tools)

(defun set-fmx-extern-location ()
  "If *FMX-EXTERN-LOCATION* is not set, ask the user to provide a
value."
  (setq *fmx-extern-location*
        (or (capi:prompt-for-file "Please select the C header file \"FMXExtern.h\":"
                                  :filters '("C Header Files" "*.h")
                                  :filter "*.h"
                                  :operation :open
                                  :ok-check (lambda (pathspec)
                                              (and (string= (file-namestring pathspec)
                                                            "FMXExtern.h"))))
            (error "I can't continue if you don't select the file \"FMXExtern.h\"."))))

(defun make-fli-type (string)
  "Converts a string like \"unsigned short\" into a corresponding
list of keywords like (:UNSIGNED :SHORT).  Returns a symbol
instead of a list if the string contained only one word."
  (let ((keyword-list
         (loop for part in (split "\\s+" string)
               collect (intern (string-upcase part) :keyword))))
    (if (cdr keyword-list) keyword-list (car keyword-list))))

(defun find-type (type &optional default)
  "Tries to find TYPE in the *TYPEDEFS* alist.  Returns DEFAULT
if nothing was found, or TYPE itself if DEFAULT is NIL."
  (or (cdr (assoc type *typedefs*)) default type))

(defun mangle-name (string &optional constantp)
  "Converts the string STRING representing a C name to a suitable Lisp
symbol in the FM-PLUGIN-TOOLS package.  Underline characters at the
beginning of a string are removed; other underline characters are
converted to hyphens; when there's a change from lowercase to
uppercase, a hyphen is inserted.  Finally, all characters are
converted to uppercase.

If CONSTANTP is true, a plus sign is added to the beginning and end of
the Lisp symbol to denote a Lisp constant."
  #+:macosx
  (when (string= string "FM_Text_AssignUnicode")
    ;; on OS X we need a wrapper around this function as
    ;; :EF-WC-STRING won't work there
    (return-from mangle-name (intern "FM-TEXT-ASSIGN-UNICODE%" :fm-plugin-tools)))
  (setq string (regex-replace-all "([a-z])([A-Z])" string "\\1-\\2")
        string (regex-replace-all "^_" string  "")
        string (regex-replace-all "_" string  "-"))
  (intern (format nil "~:[~;+~]~A~2:*~:[~;+~]"
                  constantp (string-upcase string))
          :fm-plugin-tools))

(defun type-and-name (string &optional argp)
  "Divides pairs like \"int foo\" or \"void *bar\" \(note the
asterisk) into two values - the type and the \(Lisp-mangled)
name.  Returns as the third value the name as a string.  If ARGP
is true, the result is supposed to be used as a function
argument."
  (register-groups-bind (type pointerp name)
      ("^\\s*([^*]*)(?<!\\s)(?:(\\s*\\*\\s+|\\s+\\*\\s*)|\\s+)(\\w+)\\s*$" string)
    (setq type (find-type (make-fli-type type)))
    ;; :VOID can't be passed by value - this can happen because we
    ;; often punt on types we don't know
    (if (and argp (eq type :void))
      (setq pointerp t))
    ;; (:POINTER (:UNSIGNED :SHORT)) is used for Unicode string return
    ;; values - we prefer (:POINTER :VOID) to appease LispWorks
    (if (and pointerp (equal type '(:unsigned :short)))
      (setq type :void))
    (list (if pointerp `(:pointer ,type) type)
          (mangle-name name)
          name)))

(defun simplify (line)
  "Accepts a line of C source code which is supposed to be a
function prototype and returns a simplified version."
  ;; first some know types for which we didn't bother to parse the
  ;; typedefs
  (setq line (regex-replace-all "fmx::CharacterStyle::(?:FontID|Face|FontSize|ColorChannel16)"
                                line "unsigned short")
        line (regex-replace-all "fmx::CharacterStyle::ColorChannel" line "unsigned char")
        ;; simply "convert" all the other C++ types that look
        ;; complicated to `void' - we'll deal with pointers anyway, so
        ;; it doesn't matter
        line (regex-replace-all "\\w+::[^a-z][\\w:]+" line "void")
        ;; keep the other names but without the namespace
        line (regex-replace-all "\\w+::([a-z]+)" line "\\1")
        ;; use * instead of & - we're not interested in C++ details
        line (regex-replace-all "&" line "*")
        ;; likewise, `const' isn't interesting for us
        line (regex-replace-all "const" line " ")
        ;; convert underline to hyphen for INTERN above
        line (regex-replace-all "wchar_t" line "wchar-t")
        ;; remove the C++ `throw()' part
        line (regex-replace " throw \\(\\)" line "")
        ;; this is a macro for `__declspec(dllimport)' which we also
        ;; don't need
        line (regex-replace "FMX_API" line ""))
  line)

