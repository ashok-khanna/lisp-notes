;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/functions.lisp,v 1.19 2010/07/22 09:38:06 edi Exp $

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

(in-package :fm-plugin-tools)

(define-foreign-funcallable fmx-set-to-current-env
    ((environment (:pointer :void)))
  :documentation "Updates the `FMX_ExprEnv' struct pointed to by
ENVIRONMENT with the \"current\" environment.  Probably.  This feature
is not really documented by FileMaker."
  :result-type :short)

(defun update-global-environment ()
  "Sets the global special variable *GLOBAL-ENVIRONMENT* \(used for
example as a fallback by EVALUATE) to a new `FMX_ExprEnv' struct which
is then updated by a call to FMX-SET-TO-CURRENT-ENV.  The old struct
pointed to by *GLOBAL-ENVIRONMENT* \(if any) is freed.  Must be called
from within a function that is called by FileMaker.

This function is protected with a lock, so you don't need to care
about concurrency."
  ;; update is within a lock so we don't confuse readers
  (mp:with-lock (*global-environment-lock*)
    (let (old-global-environment done)
      (unwind-protect
          (progn
            (setq old-global-environment *global-environment*
                  *global-environment* (fm-expr-env-constructor1))
            ;; update
            (fmx-set-to-current-env (c-current-env) *global-environment*)
            (setq done t))
        (when old-global-environment
          (ignore-errors
            ;; free old struct
            (fm-expr-env-delete old-global-environment)))
        (unless done
          ;; try to free if something went wrong
          (ignore-errors
            (fm-expr-env-delete *global-environment*))
          (setq *global-environment* nil))))))

(defgeneric evaluate (expression &optional result)
  (:documentation "Evaluates \(as with FileMaker's `Evaluate'
function) the expression EXPRESSION.  The result is stored in the
DATA-OBJECT RESULT which is created if none is provided.  The
function returns RESULT."))

(defmethod evaluate ((expression text-object) &optional (result (make-data-object)))
  (let ((err-code (fm-expr-env-evaluate (get-environment)
                                        (pointer expression)
                                        (pointer result))))
    (unless (zerop err-code)
      (error "Got error code ~A while evaluating ~S."
             err-code (as-string expression))))
  result)

(defmethod evaluate ((expression string) &optional (result (make-data-object)))
  (evaluate (make-text-object expression) result))

(defgeneric execute-sql (expression column-separator row-separator &optional result)
  (:documentation "Executes \(as with FileMaker's `ExecuteSQL') the
expression EXPRESSION using the column separator COLUMN-SEPARATOR and
the row separator ROW-SEPARATOR \(both of which must be characters).
The result is stored in the DATA-OBJECT RESULT which is created if
none is provided.  The function returns RESULT."))

(defmethod execute-sql ((expression text-object) column-separator row-separator
                        &optional (result (make-data-object)))
  (let ((err-code (fm-expr-env-execute-sql (get-environment)
                                           (pointer expression)
                                           (pointer result)
                                           (char-code column-separator)
                                           (char-code row-separator))))
    (unless (zerop err-code)
      (error "Got error code ~A while executing SQL ~S with column separator ~S and row separator ~S."
             err-code (as-string expression) column-separator row-separator)))
  result)

(defmethod execute-sql ((expression string) column-separator row-separator
                        &optional (result (make-data-object)))
  (execute-sql (make-text-object expression) column-separator row-separator result))

(defun set-value (value &key (target *results*) result-type)
  "Sets the contents of the DATA-OBJECT TARGET to VALUE.  If
TARGET is not provided, the return value of the currently
executing plug-in function is set.  RESULT-TYPE can be one
of :BOOLEAN, :DATE, :TIME, :TIMESTAMP, :UNIVERSAL-TIME describing
the intended FileMaker type of VALUE.  RESULT-TYPE can also be
NIL in which case the function tries to do the right thing
depending on the Lisp type of VALUE.  Finally, RESULT-TYPE can
be :VOID which means that this function does nothing."
  (ecase result-type
    (:void)
    (:boolean
     (with-fix-pt (fix-pt-ptr (if value 1 0))
       (fm-data-set-as-number (pointer target) fix-pt-ptr +k-dtnumber+)))
    (:date
     (unless (typep value 'date-time-object)
       (error "Value ~S should have been a DATE-TIME-OBJECT for result type :DATE."
              value))
     (fm-data-set-as-date (pointer target) (pointer value) +k-dtdate+))
    (:time
     (unless (typep value 'date-time-object)
       (error "Value ~S should have been a DATE-TIME-OBJECT for result type :TIME."
              value))
     (fm-data-set-as-time (pointer target) (pointer value) +k-dttime+))
    (:timestamp
     (unless (typep value 'date-time-object)
       (error "Value ~S should have been a DATE-TIME-OBJECT for result type :TIMESTAMP."
              value))
     (fm-data-set-as-time-stamp (pointer target) (pointer value) +k-dttime-stamp+))
    (:universal-time
     (let ((date-time (make-date-time-object :universal-time value)))
       (fm-data-set-as-time-stamp (pointer target) (pointer date-time) +k-dttime-stamp+)))
    ((nil)
     (typecase value
       (data-object)
       (string
        (with-text (text-ptr value)
          (fm-data-set-as-text* (pointer target) text-ptr)))
       ((or integer float)
        (with-fix-pt (fix-pt-ptr value)
          (fm-data-set-as-number (pointer target) fix-pt-ptr +k-dtnumber+)))
       (text-object
        (fm-data-set-as-text* (pointer target) (pointer value)))
       (fix-pt-object
        (fm-data-set-as-number (pointer target) (pointer value) +k-dtnumber+))
       (binary-data-object
        (fm-data-set-binary-data (pointer target) (pointer value) t))
       (otherwise
        ;; boolean
        (with-fix-pt (fix-pt-ptr (if value 1 0))
          (fm-data-set-as-number (pointer target) fix-pt-ptr +k-dtnumber+))))))
  target)

(define-foreign-funcallable fmx-start-script
    ((file-name (:pointer :void))
     (script-name (:pointer :void))
     (control (:unsigned :char))
     (parameter (:pointer :void)))
  :documentation "Starts the FileMaker script with the name
SCRIPT-NAME in the file FILE-NAME.  See the FileMaker documentation
for the meaning of the CONTROL and PARAMETER arguments."
  :result-type :short)

(defgeneric start-script (file-name script-name &key control parameter)
  (:documentation "Starts the FileMaker script with the name
SCRIPT-NAME in the file FILE-NAME.  PARAMETER can be a
DATA-OBJECT or any other object that can be converted
automatically to a DATA-OBJECT with SET-VALUE.  See the FileMaker
documentation for the meaning of the CONTROL and PARAMETER
arguments."))

(defmethod start-script ((file-name text-object) (script-name text-object)
                         &key (control +k-fmxt-pause+) parameter)
  (when (and parameter (not (typep parameter 'data-object)))
    (let ((data-object (make-data-object)))
      (set-value parameter :target data-object)
      (setq parameter data-object)))
  (unless (zerop
           (fmx-start-script (c-start-script) (pointer file-name) (pointer script-name)
                             control (if parameter (pointer parameter) *null-pointer*)))
    (error "An error occurred while trying to execute script ~S in ~S."
           (as-string script-name) (as-string file-name)))
  (values))

(defmethod start-script ((file-name string) script-name
                         &key (control +k-fmxt-pause+) parameter)
  (start-script (make-text-object file-name) script-name
                :control control :parameter parameter))

(defmethod start-script (file-name (script-name string)
                         &key (control +k-fmxt-pause+) parameter)
  (start-script file-name (make-text-object script-name)
                :control control :parameter parameter))

(defun enlist-plugin-function (prototype c-name min-args max-args flags)
  "Adds a new plug-in function with the corresponding parameters
to the list *PLUGIN-FUNCTIONS*."
  (pushnew (list (next-function-id)
                 prototype
                 c-name
                 min-args
                 max-args
                 flags)
           *plugin-functions*
           :key (lambda (tuple)
                  (function-name (second tuple)))
           :test #'string=))

(defun nth-arg (n &optional type)
  "Returns the Nth argument \(if there is one) of the currently
executing plug-in function.  TYPE determines how the argument
should be returned and must be one of :TEXT \(for a
TEXT-OBJECT), :STRING \(for a Lisp string), :FIX-PT \(a
FIX-PT-OBJECT), :INTEGER \(a Lisp integer), :FLOAT \(a Lisp
float), :BOOLEAN \(a Lisp boolean), :DATE, :TIME, :TIMESTAMP
\(DATE-TIME-OBJECTs), :UNIVERSAL-TIME \(a Lisp universal
time), :BINARY-DATA \(a BINARY-DATA-OBJECT), or NIL \(the
DATA-OBJECT itself)."
  (when (< n (fm-data-vect-size *args*))
    (case type
      (:text (make-instance 'text-object
                            :pointer (fm-data-vect-at-as-text *args* n)
                            :do-not-delete t))
      (:string (fm-text-get-string (fm-data-vect-at-as-text *args* n)))
      (:fix-pt (make-instance 'fix-pt-object
                             :pointer (fm-data-vect-at-as-number *args* n)
                             :do-not-delete t))
      (:integer (fm-fix-pt-as-long (fm-data-vect-at-as-number *args* n)))
      (:float (fm-fix-pt-as-float (fm-data-vect-at-as-number *args* n)))
      (:boolean (fm-data-vect-at-as-boolean *args* n))
      (:date (make-instance 'date-time-object
                            :pointer (fm-data-vect-at-as-date *args* n)
                            :do-not-delete t))
      (:time (make-instance 'date-time-object
                            :pointer (fm-data-vect-at-as-time *args* n)
                            :do-not-delete t))
      (:timestamp (make-instance 'date-time-object
                                 :pointer (fm-data-vect-at-as-time-stamp *args* n)
                                 :do-not-delete t))
      (:universal-time (as-universal-time
                        (make-instance 'date-time-object
                                       :pointer (fm-data-vect-at-as-time-stamp *args* n)
                                       :do-not-delete t)))
      (:binary-data (make-instance 'binary-data-object
                                   :pointer (fm-data-vect-at-as-binary-data *args* n)
                                   :do-not-delete t))
      ((nil) (make-instance 'data-object
                            :pointer (fm-data-vect-at *args* n)
                            :do-not-delete t)))))

(defun create-bindings (lambda-list)
  "Accepts a lambda list as for DEFINE-PLUGIN-FUNCTION, checks
it, and returns a list of corresponding LET bindings."
  (let ((counter 0)
        (state :required)
        (bindings nil)
        (min-args 0)
        (max-args nil))
    (dolist (thing lambda-list)
      (case thing
        (&optional
         (unless (eq state :required)
           (error "Unexpected &OPTIONAL in lambda list."))
         (setq state :optional))
        (&rest
         (unless (member state '(:required :optional))
           (error "Unexpected &REST in lambda list."))
         (setq state :rest))
        (otherwise
         (when (eq state :done)
           (error "Only one parameter may follow &REST."))
         (when (atom thing)
           (unless (symbolp thing)
             (error "Expected symbol in lambda list but got ~S." thing))
           (setq thing (list thing nil)))
         (unless (and (or (eql 2 (list-length thing))
                          (and (eq state :optional)
                               (eql 3 (list-length thing))))
                      (symbolp (first thing))
                      (member (second thing) '(:text :string :fix-pt :integer :float :boolean :date
                                               :time :timestamp :universal-time :binary-data nil)))
           (error "Illegal parameter specifier ~S." thing))
         (push (list (first thing)
                     (case state
                       (:rest
                        (with-unique-names (i)
                          `(loop for ,i from ,counter below (fm-data-vect-size *args*)
                                 collect (nth-arg ,i ,(second thing)))))
                       (otherwise
                        ;; note that (THIRD THING) is NIL for required arguments
                        `(or (nth-arg ,counter ,(second thing)) ,(third thing)))))
               bindings)
         (incf counter)
         (when (eq state :required)
           (incf min-args))
         (when (eq state :rest)
           (setq state :done)))))
    (when (eq state :rest)
      (error "No parameter following &REST."))
    (unless (eq state :done)
      (setq max-args counter))
    (values (nreverse bindings) min-args max-args)))

(defmacro define-plugin-function (description lambda-list &body body)
  "Defines a plug-in function.  DESCRIPTION is either a string
with the function prototype as it should be shown by FileMaker or
a list where the first element is this prototype string followed
by a plist.

The plist can have the properties :MAX-ARGS, :FLAGS,
and :RESULT-TYPE.  MAX-ARGS is the maximal number of arguments
for the function.  It will only be used if there's a &REST
parameter in the lambda list.  RESULT-TYPE will be interpreted as
by SET-VALUE.  FLAGS is a boolean combination of flags describing
the behaviour of the function - see the FileMaker documentation.

LAMBDA-LIST is like a simplified version of a Lisp lambda list
where only &OPTIONAL and &REST are allowed.  Each parameter is
either a symbol or a pair \(NAME TYPE) where TYPE is interpreted
as by NTH-ARG.  Optional parameters can also look like \(NAME
TYPE DEFAULT-VALUE)."
  ;; we want to always treat DESCRIPTION like a list
  (when (atom description)
    (setq description (list description)))
  (let ((prototype (first description))
        (result-type (getf (rest description) :result-type))
        (callable-name (next-callable-name)))
    (multiple-value-bind (bindings min-args max-args)
        (create-bindings lambda-list)
      (with-unique-names (func-id result results cond error-occurred)
        `(progn
           ;; create the actual C stub which will be called by
           ;; FileMaker
           (define-foreign-callable (,callable-name :result-type :short
                                                    :calling-convention :stdcall)
               ((,func-id :short)
                ;; bind special variables
                (*environment* (:pointer :void))
                (*args* (:pointer :void))
                (,results (:pointer :void)))
             (declare (ignore ,func-id))
             (catch ',error-occurred
               (handler-bind
                   ((error (lambda (,cond)
                             (maybe-log-error ,cond ,prototype)
                             ;; return -1 to FileMaker in case of an error
                             (throw ',error-occurred -1))))
                 (let* ((*results* (make-instance 'data-object
                                                  :pointer ,results
                                                  :do-not-delete t))
                        ,@bindings
                        (,result (progn ,@body)))
                   ,@(unless (eq result-type :void)
                       ;; set return value
                       `((set-value ,result :result-type ,result-type)))
                   ;; return 0 for "no error"
                   0))))
           ;; the name of the callable must be kept if the delivery
           ;; level is 5
           (push ',callable-name *symbols-to-keep*)
           ;; enlist function so it will be registered when the
           ;; plug-in is initialized
           (enlist-plugin-function ,prototype
                                   ',callable-name
                                   ,min-args
                                   ,(or max-args (getf (rest description) :max-args))
                                   ,(getf (rest description) :flags)))))))
