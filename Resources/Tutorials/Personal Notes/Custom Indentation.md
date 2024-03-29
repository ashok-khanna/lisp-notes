# Custom Indentation in Emacs

***Note**: Reddit user Aidenn0 kindly pointed out "Note that with slime/sly it will indent the macro properly so long as the macro is already defined in the image you are debugging.", which renders the below unnecessary in many instances :-) However, perhaps it still has some instructional value.*

Today we will briefly illustrate how to customise the indentation of your Common Lisp macros within Emacs.

We will customise the indentation of a user-defined WITH macro as an example. I wrote the WITH macro to streamlime some of my LET forms where I was sharing variable and function names as illustrated in the following example.

## Example of With Macro

```lisp

;; A sample LET form:

(defun print-car-details (car)
  (let ((car-color (car-color car))
        (car-model (car-model car)))
    (format t "Car color of model ~a is ~a" car-model car-color)))
    
;; An equivalent WITH form:

(defun print-car-details (car)
  (with ((car-color car)
         (car-model car)))
    (format t "Car color of model ~a is ~a" car-model car-color)))
```

## Incorrect Default Indentation

However, without prior knowledge of this macro, Emacs would indent it undesirably as follows.

```lisp
(defun print-car-details (car)
  (with ((car-color car)
         (car-model car)))
        (format t "Car color of model ~a is ~a" car-model car-color)))
```

## The Fix

Fortunately, the fix is relatively easy and the answer lies in [cl-indent.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/cl-indent.el). We simply need to evaluate the following Elisp code, or better yet, add the below to our Emacs configuration settings.

```lisp
(put 'with 'common-lisp-indent-function '((&whole 4 &rest (&whole 1 1 2)) &body))
```

Whilst this looks complicated, there is a simple trick. Simply read [cl-indent.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/cl-indent.el) and copy/paste its indentation rules from a relevant form whose indentation we wish to replicate. For example, [the indentation for a LET form is what we need here](https://github.com/emacs-mirror/emacs/blob/8d53c23f90aab6e527c61137ae43274c7a36eca7/lisp/emacs-lisp/cl-indent.el#L787).

## Correct Customised Indentation
Below is what our WITH macro looks like after customising its indentation. Looks much better now :-)

```lisp
(defun print-car-details (car)
  (with ((car-color car)
         (car-model car)))
    (format t "Car color of model ~a is ~a" car-model car-color)))
```

And that concludes our short guide! You may need some more complex functionality, in which case some of the below references may be useful[^1][^2][^3].

## Appendix: The With Macro
```lisp
(defmacro with (bindings &body body)
  "Equivalent to a let form, with the function name in the binding calls used also as the variable name."
    `(let ,(make-bindforms bindings)
       ,@body))

(defmacro with* (bindings &body body)
  "Equivalent to a let* form, with the function name in the binding calls used also as the variable name."
    `(let* ,(make-bindforms bindings)
       ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)          
  (defun make-bindforms (bindforms)
    "Transform a list of function calls (fn args) to a list of (fn (fn args))."
    (mapcar #'(lambda (b) (list (car b) b)) bindforms)))
```

[^1]: Why does Emacs indent my lisp loop construct weirdly: https://emacs.stackexchange.com/questions/30788/why-does-emacs-indent-my-lisp-loop-construct-weirdly
[^2]: Trivial Indent: https://shinmera.github.io/trivial-indent/
[^3]: How to use custom macro indentation in Elisp: https://emacs.stackexchange.com/questions/34520/how-to-use-custom-macro-indentation-in-elisp
