(in-package :utilities)

(defun list-sum (my-list)
  (reduce #'+ my-list))

(defun list-product (my-list)
  (reduce #'* my-list))
