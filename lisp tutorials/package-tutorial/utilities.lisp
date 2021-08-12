(in-package :utilities)

(defun list-sum (my-list)
  (reduce #'+ my-list))
