;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PREPARE-FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/prepare-fm-plugin-tools/prepare.lisp,v 1.19 2010/07/22 09:38:10 edi Exp $

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

(defun handle-typedef (line)
  "Accepts a string which is supposed to be a simple C typedef.
Stores a corresponding entry in *TYPEDEFS*."
  (when (scan "typedef(?!.*[A-Z_])" line)
    (register-groups-bind (existing-type defined-type)
        ("typedef\\s+(.*)(?<!\\s)\\s+(\\w+);" line)      
      (pushnew (cons (make-fli-type defined-type)
                     (make-fli-type existing-type))
               *typedefs*
               :key #'car))))

(defun read-enum-value (string)
  "Reads the optional value part of a C enum and returns a
corresponding Lisp value - either a number or a LOGIOR
expression."
  ;; convert hex marker for Lisp reader
  (setq string (regex-replace-all "0x" string "#x"))
  (if (scan "\\|" string)
    ;; contains a pipe symbol, so make LOGIOR of previously defined
    ;; constants
    (let (result)
      (do-matches-as-strings (value "\\w+" string)
        (push (mangle-name value t) result))
      (cons 'logior (nreverse result)))
    ;; just read value as a number
    (read-from-string string)))

(defun write-function-definition (lisp-name c-name result-type args)
  "Accepts values which suffice to create a foreign function
defintion and writes it to the output stream."
  ;; we use DEFINE-FMXCPT-FUNCTION as defined in FM-PLUGIN-TOOLS
  (pprint `(fm-plugin-tools::define-fmxcpt-function (,lisp-name ,c-name)
               ,(loop for (type name nil) in (butlast args)
                      collect `(,name ,(if (and #-:win32 nil
                                                (string= c-name "FM_Text_AssignUnicode")
                                                (string-equal name "s"))
                                         ;; special case for this one function (only on Windows) -
                                         ;; pass Lisp string directly as an argument
                                         '(:reference-pass (:ef-wc-string :external-format :unicode))
                                         type)))
             :result-type ,result-type)))

(defun handle-function (line)
  "Accepts one line of C code and checks if it's a function prototype.
If it is one, we write a corresponding function definition to the
output stream."
  ;; all `interesting' prototypes use the FMX_API macro - we just have
  ;; to throw away the lines where this macro is defined
  (when (and (scan "FMX_API.*&_x" line)
             (not (scan "#define" line)))
    (setq line (simplify line))
    ;; the part between the parens are the arguments - that's
    ;; simple... :)
    (register-groups-bind (head args)
        ("(.*)\\((.*)\\);" line)
      (destructuring-bind (result-type lisp-name c-name)
          (type-and-name head)
        (write-function-definition lisp-name c-name result-type
                                   ;; args are separated by commas
                                   (loop for arg in (split "," args)
                                         collect (type-and-name arg t)))))))

(defun handle-enum (body)
  "Handles the part between `enum {' and `}'.  Loops through all
lines, writes one DEFCONSTANT per item and the corresponding
EXPORT statement."
  (let ((counter 0))
    (do-register-groups (name value)
        ("(?m)^\\s*(\\w+)\\s*(?:=\\s*([^,/]*\\z|.*?)\\s*)?(?:,|(?:/.*)?$)" body)
      ;; use value if provided in enum, COUNTER otherwise
      (setq value (if value (read-enum-value value) counter))
      (let ((lisp-name (mangle-name name t)))
        (pprint `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (defconstant ,lisp-name ,value
                     "This constant was generated automatically.
See FileMaker header files for details.")))
        (print `(export ',lisp-name :fm-plugin-tools)))
      ;; increment counter or continue with successor of value
      (setq counter (1+ (if (numberp value) value counter))))))

(defun handle-struct (struct-name body pack)
  "Handles the part between `struct {' and `}' - writes a
corresponding FLI:DEFINE-C-STRUCT definition.  If PACK is true,
byte-packing will be used."
  (let (slots)
    (do-register-groups (type name)
        ;; for some reason FMX_PACK isn't used in 8.5 anymore
        ("(?m)^\\s*(\\w+)\\s+(\\w+)(?:\\s*FMX_PACK)?\\s*;(?:\\s*//.*)?\\s*?$" body)
      (push (list (if (scan "FMX_" type)
                    ;; default types which start with `FMX_' to :VOID
                    (find-type (make-fli-type (regex-replace "FMX_" type ""))
                               '(:pointer :void))
                    (find-type (make-fli-type type)))
                  (mangle-name name)
                  pack)
            slots))
    (pprint `(fli:define-c-struct ,(mangle-name struct-name)
               ,@(loop for first = t then nil
                       for (slot-type slot-name packp) in (nreverse slots)
                       when (and packp (not first))
                       collect '(:byte-packing 1)
                       collect `(,slot-name ,(if (and (string= struct-name "FMX_ExternCallStruct")
                                                      (string-equal slot-name "which-call"))
                                               ;; special for this one slot
                                               '(:unsigned :char)
                                               slot-type)))))))

(defun parse-header-files ()
  "Loops through all C header files in *HEADER-FILE-NAMES*,
checks for enums, structs or function prototypes and writes the
corresponding C code to *STANDARD-OUTPUT*."
  (dolist (name *header-file-names*)
    (let* ((header-file (make-pathname :name name
                                      :defaults *fmx-extern-location*))
           (file-string (file-string header-file)))
      (with-open-file (in header-file)
        (loop for line = (read-line in nil nil)
              while line
              do (handle-typedef line)
                 (handle-function line)))
      (do-register-groups (enum-body)
          ("(?s)enum(?:\\s+\\w+)?\\s*\\{\\s*(.*?)\\s*,?\\s*\\}" file-string)
        (handle-enum enum-body))
      (do-register-groups (pack name whitespace struct-body)
          ;; FMX_PACK_ON is a macro which translates to (:BYTE-PACKING 1)
          ("(?sm)(#pragma\\s+FMX_PACK_ON\\s+)?^\\s*struct (\\w+)$(\\s*){(.*?)\\3}" file-string)
        (declare (ignore whitespace))
        (handle-struct name struct-body pack)))))

(defun prepare ()
  "Creates the missing file `fli.lisp' for FM-PLUGIN-TOOLS from
the C header files of FileMaker Pro Advanced."
  ;; find out where to look for headers
  (unless *fmx-extern-location*
    (set-fmx-extern-location))
  ;; redirect *STANDARD-OUTPUT* to `fli.lisp'
  (with-open-file (*standard-output* *fli-file*
                                     :direction :output
                                     :if-exists :supersede)
    ;; use correct package for output and refrain from writing
    ;; everything in uppercase
    (with-standard-io-syntax 
      (let ((*package* (find-package :fm-plugin-tools))
            (*print-case* :downcase))
        (format t ";;; this file was generated automatically~%")
        (print '(in-package :fm-plugin-tools))
        (terpri)
        ;; let this function do all the work
        (parse-header-files))))
  :done)