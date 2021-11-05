;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CAPI-OVERVIEW; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/capi-overview/capi-classes.lisp,v 1.7 2007/01/01 23:38:50 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz. All rights reserved.

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

(in-package :capi-overview)

(defclass capi-class ()
     ((name :initarg :name
            :accessor name
            :documentation "The name of the class - a symbol.")
      (level :initform nil
             :accessor level
             :documentation "The level of the class which is
defined to be one more than the maximum level of all superclasses
which are also external in the CAPI or GP package.  If there are
no superclasses the level is 0."))
  (:documentation "An object of type CAPI-CLASS describes an
external class from the CAPI or GP package."))

(defun interesting-symbols ()
  "Returns a list of all external symbols from the CAPI and GP
packages which denote classes."
  (append
   (loop for s being the external-symbols of :capi
         when (find-class s nil) 
           collect s)
   (loop for s being the external-symbols of :gp
         when (find-class s nil) 
           collect s)))

(defun interestingp (symbol)
  "Returns a true value iff SYMBOL is an external symbol of one
of the packages CAPI or GP."
  (or (and (eql (symbol-package symbol)
                #.(find-package :capi))
           (eq (nth-value 1 (find-symbol (symbol-name symbol) :capi))
               :external))
      (and (eql (symbol-package symbol)
                #.(find-package :gp))
           (eq (nth-value 1 (find-symbol (symbol-name symbol) :gp))
               :external))))

(defun sub/superclasses (symbol sub)
  "Helper function which recursively walks the superclasses or
the subclasses of the class denoted by the symbol SYMBOL until a
class which is `interesting' with respect to INTERESTINGP is
found.  The names of these classes are returned.  The generalized
boolean SUB denotes whether we want to have subclasses or
superclasses."
  (loop for sub/sup-class in (funcall (if sub
                                        #'hcl:class-direct-subclasses
                                        #'hcl:class-direct-superclasses)
                                      (find-class symbol))
        for sub/sup-name = (class-name sub/sup-class)
        when (interestingp sub/sup-name)
          collect sub/sup-name
        else
          append (sub/superclasses sub/sup-name sub)))

(defun subclasses (symbol)
  "Returns the `direct' subclasses of the class denoted by the
symbol SYMBOL.  \(These subclasses are direct with respect to the
class tree that consists only of `interesting' classes.)"
  (sub/superclasses symbol t))

(defun superclasses (symbol)
  "Returns the `direct' superclasses of the class denoted by the
symbol SYMBOL.  \(These superclasses are direct with respect to
the class tree that consists only of `interesting' classes.)"
  (sub/superclasses symbol nil))

(defun subclasses* (symbol)
  "Returns all `interesting' subclasses of the class denoted by
the symbol SYMBOL."
  (let ((subclasses (subclasses symbol)))
    (cond (subclasses (append subclasses
                              (mapcan #'subclasses* subclasses)))
          (t nil))))

(defun superclasses* (symbol)
  "Returns all `interesting' superclasses of the class denoted by
the symbol SYMBOL."
  (let ((superclasses (superclasses symbol)))
    (cond (superclasses (append superclasses
                                (mapcan #'superclasses* superclasses)))
          (t nil))))

(defun make-level (capi-class)
  "Sets and returns the level of the CAPI-CLASS object
CAPI-CLASS.  Calls itself recursively if needed."
  (let ((superclasses (superclasses (name capi-class))))
    (setf (level capi-class)
            (cond ((null superclasses) 0)
                  (t (let ((super-levels (loop for super in superclasses
                                               collect (make-level (capi-class super)))))
                       (1+ (apply #'max super-levels))))))))

(defun all-capi-classes ()
  "Sets \(and returns) the special variable *ALL-CAPI-CLASSES* to
a sorted list of CAPI-CLASS objects wrapping all `interesting'
CAPI and GP classes."
  (setq *all-capi-classes* (loop for symbol in (sort (interesting-symbols)
                                                     #'string<)
                                 collect (make-instance 'capi-class
                                            :name symbol)))
  (loop for capi-class in *all-capi-classes*
        unless (level capi-class)
          do (make-level capi-class))
  *all-capi-classes*)

(defun capi-class (symbol)
  "Returns the CAPI-CLASS object corresponding to the class named
by the symbol SYMBOL.  Assumes that *ALL-CAPI-CLASSES* has been
set by a previous call to ALL-CAPI-CLASSES."
  (find symbol *all-capi-classes* :key #'name))
