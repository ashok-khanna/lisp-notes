;;;;****************************************************************************
;;;; -*- coding:utf-8 -*-
;;;; Author: Ashok Khanna
;;;; License: Public Domain
;;;;
;;;;****************************************************************************

;; Best to run each of these forms manually
;; so that you can work through the examples
;; and see their results

;;;;****************************************************************************

(defpackage "MIXIN-EXAMPLE"
  (:use :cl))

;;;;****************************************************************************

;;; Part 1 of Article

(defclass fundamental-test ()
  ((test-form :initarg :form
              :accessor test-form)
   (test-value :initarg :value
               :accessor test-value)
   (test-result :initform nil
                :accessor test-result)
   (test-equal :initform #'equal
               :initarg :test
               :accessor test-equal)))

(defgeneric run-test (obj))

(defmethod run-test ((obj fundamental-test))
  (if (funcall (test-equal obj)
               (apply (car (test-form obj))
                      (cdr (test-form obj)))
               (test-value obj))
      t
      nil))

;; Try the following

(defparameter *test-1*
  (make-instance 'fundamental-test
		 :form '(+ 4 5)
		 :value 9))

;; Should return 'T':

(run-test *test-1*)

;;;;****************************************************************************

;;; Part 2 of Article

(defclass hash-mixin ()
  ((key :initarg :key :accessor key)
   (table :accessor table
          :allocation :class
          :initform (make-hash-table :test #'equal))))

(defclass test-hash-mixin (hash-mixin)
  ((hash-table :accessor table :allocation :class
               :initform (make-hash-table :test #'equal))))

;;;;****************************************************************************

;;; Part 3 of Article

(defclass unit-test (test-hash-mixin fundamental-test) ())

;;;;****************************************************************************

;;; Part 4 of Article

;;; 4.1 Creation

(defmethod initialize-instance :after ((obj hash-mixin) &key)
  (setf (gethash (key obj) (table obj)) obj))

;; Let's test it out (also show how hash tables differ between HASH-MIXIN
;; and TEST-HASH-MIXIN)

(defparameter *test-2*
  (make-instance 'unit-test
		 :form '(+ 4 5)
		 :value 9
		 :key "test-2"))

;; Should return a UNIT-TEST instance:

(gethash "test-2" (table *test-2*))

;; This also works:

(gethash (key *test-2*) (table *test-2*))

;; Now lets create another class that only uses HASH-MIXIN:

(defclass book (hash-mixin)
  ((title :initarg :title)))

(defparameter *book-1*
  (make-instance 'book
		 :title "Common Lisp the Language, 2nd Edition"
		 :key "Guy Steele"))

;; Now try the following

(gethash (key *book-1*) (table *book-1*)) ; > Returns a book

(gethash (key *book-1*) (table *test-2*)) ; > Returns nil

(gethash (key *test-2*) (table *book-1*)) ; > Returns nil

;;;;****************************************************************************

;;; 4.2 Reading

(defun read-test (obj)
  (gethash (key obj) (table obj)))

;; Returns a test object:

(read-test *test-2*)

;;;;****************************************************************************

;;; 4.3 Updating

(defmethod (setf key) :before (key (obj hash-mixin))
  (setf (gethash (key obj) (table obj)) nil))

(defmethod (setf key) :after (key (obj hash-mixin))
  (setf (gethash (key obj) (table obj)) obj))

(setf (key *test-2*) "New Key")

;; Try these now:

(gethash "test-2" (table *test-2*)) ; > Returns Nil

(gethash "New Key" (table *test-2*)) ; > Returns a test object

;;;;****************************************************************************

;;; 4.4 Deleting

(defun delete-test (obj)
  (setf (gethash (key obj) (table obj)) nil))

;; Try the following

(defparameter *test-3*
  (make-instance 'unit-test :form '(+ 4 10)
			    :value 14
			    :key "test-3"))

;; Returns a test object (note that I accessed the hash table linked to *test-2* --> *test-2* and *test-3*
;; and all test objects share the same hash table

(gethash "test-3" (table *test-2*))

;; Now try this

(delete-test *test-3*)

(gethash "test-3" (table *test-2*)) ; > Returns Nil
