(in-package :cl)

(defpackage :utilities
  (:use :cl)
  (:export :list-sum))

(defpackage :quant
  (:use :cl :utilities)
  (:export :csv-file))

(defpackage :main
  (:use :cl :quant))

;; Change filepath to where you save your main.lisp:
(load "/Users/ashokkhanna/main.lisp")
