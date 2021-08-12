# The Common Lisp Cheat Sheet

1. [The Basics](#org77d32bb)
   1. [Global Variables](#org7babf58)
   2. [Global Functions](#org7561355)
   3. [Assignment](#orgaf86fa8)
   4. [Input & Output](#orgd660dc0)
   5. [Numerical Functions](#org14d1d98)
   6. [Text Functions](#org509a49b)
2. [Logic & Equality](#orgbb85d6f)
   1. [Type Predicates](#org3d5c207)
   2. [Boolean & Logic](#org2ea5239)
   3. [Equality](#org95cc4a3)
   4. [Blocks](#orgf591e22)
   5. [Conditionals](#org3a0d565)
3. [Looping](#orge0a634f)
   1. [Basic Looping](#org436ea7c)
   2. [Advanced Looping](#org83b693d)
4. [Local Variables & Functions](#org92b85fc)
   1. [Local Variables](#org9f76fb8)
   2. [Local Functions](#org9b010ba)
5. [More on Functions](#org283acf2)
   1. [Lambda Expressions](#orgfceb856)
   2. [Function Parameters](#org7a4f5d3)
   3. [Multiple Values](#org86b4d87)
   4. [Apply & Funcall](#orgc982106)
   5. [Mapping Functions](#orgc1e2096)
6. [More on Lists](#org721d229)
   1. [List Functions](#org681ae0f)
   2. [Push, Pop & Reverse](#orgc1be892)
   3. [Association Lists](#org34d47a7)
7. [More on Sequences](#org4faae7d)
   1. [Arrays](#org6377bc9)
   2. [Sequence Functions](#org469c85e)
   3. [Keyword Arguments](#org5e7bf39)
8. [Data Structures](#org94ffa30)
   1. [Hash Tables](#orgb7bdb2a)
   2. [Structures](#orge20b38c)
   3. [Common Lisp Object System (CLOS)](#orgab67257)
9. [Other](#orge334e2e)
   1. [Reading & Writing to Files](#org5926aaf)
   2. [Packages](#orgc750fcf)


<a id="org77d32bb"></a>

# The Basics


<a id="org7babf58"></a>

## Global Variables

We can define global variables with `DEFPARAMETER` and `DEFVAR`. `DEFPARAMETER` will always bind the supplied value, while `DEFVAR` will only bind a value to a variable if no binding exists.

```lisp

;; Establish an unbound variable

(defvar *x*)

;; Assign the value 15 to X

(defparameter *x* 15)

;; Does nothing as X already bound

(defvar *x* 10)

```

We can define global constants with `DEFCONSTANT`:

```lisp

(defconstant +my-constant+ 20)

```


<a id="org7561355"></a>

## Global Functions

Global functions are defined with `DEFUN` using the below syntax.

```lisp

(defun function-name (parameter*)
  "Optional documentation string."
  body-form*)

```

Below is example of a function that multiplies the sum of two numbers by 10.

```lisp

(defun multiply-sum-by-10 (x y)
  "Returns the sum of two numbers multiplied by 10"
  (* 10 (+ x y)))

;; Returns 150

(multiply-sum-by-10 5 10)

```


<a id="orgaf86fa8"></a>

## Assignment

`SETF` is LISP's general purpose assignment operator. It is a powerful setter that can assign values to many different types of places.

The syntax of `SETF` is as follows. Places are typically either symbols, representing variables or functions, or getter forms that access particular places in objects that we wish to modify.

```lisp

(setf place value)

```

Below are some examples.

```lisp

;; Set x to 10

(setf x 10)

;; Set x to 1 and y to 2

(setf x 1 y 2)

;; Example of using a getter with setf

(defvar *list* '(1 2 3 4))

(setf (car *list*) 5)

;; Returns (5 2 3 4)

*list*

```


<a id="orgd660dc0"></a>

## Input & Output

The LISP reader consists of three primary functions

-   The function `READ` is used to parse input into Lisp objects and reads exactly one expression, regardless of newlines

-   The function `READ-LINE` reads all characters up to a newline, returning a string

-   The function `READ-FROM-STRING` takes a string and returns the first expression from it

Below are some examples you can experiment with.

```lisp

(defparameter my-variable nil)

(setf my-variable (read-line))

(setf my-variable (read))

(setf my-variable (read-from-string "(1 2 3)"))

```

The most useful LISP printing functions are:

-   `PRINT` and `PRIN1` are used to generate output useful for programs (the former adds a newline to the output whilst the latter does not)

-   `PRINC` is used for output for people

-   `FORMAT` is a highly configurable printer and the most commonly used printer

The syntax of `FORMAT` is as follows:

```lisp

(format destination control-string optional-arguments*)

```

The first argument of the `FORMAT` function is the destination where the output will be printed.

-   A value of `T` will send the out to the stream `*​STANDARD-OUTPUT​*` (typically the main screen of your LISP system) whilst `NIL` here will return the output as a string. We can also supply a stream here
-   The second argument is the string that we want to print. However, we can enter directives (preceded by `~`) to add complex behaviour to the string, such as
    -   `~%` to print newlines
    -   `~A` to print LISP objects for humans
    -   `~S` to print LISP objects for computers, i.e. suitable as as input for the LISP reader

The third (optional) argument of the `FORMAT` function is the arguments we want to supply to the control string. Each `~A` or `~S` in the control-string takes a successive argument from here and places it into the string.

This is best illustrated by the following examples. Note how Bob is quoted in the second example, as the printed representation of LISP strings includes quotes.

```lisp

;; Prints to screen: Dear Bob, How are you?

(format t "Dear ~A, ~% How are you?" "Bob")

;; Prints to screen: Dear "Bob", How are you?

(format t "Dear ~S, How are you?" "Bob")

;; Returns "Number is: 3" (a string)

(format nil "~A ~A" "Number is:" (+ 1 2))

```


<a id="org14d1d98"></a>

## Numerical Functions

Basic numerical functions include `+`, `*`, `-`, `/`. They can take more than two operands.

```lisp

;; Returns 20

(+ 4 7 9)

```

Numerical comparisons can be achieved with `=`, `/=` (not equal to), `>`, `<`, `>=` and `<=`. With three or more arguments, these functions act as range checks.

For example, the below returns true as X is between 0 and 5 inclusive.

```lisp

(defparameter x 5)

(<= 0 x 5)

```

The below returns false as X > Y:

```lisp

(defparameter y 4)

(< 0 x y 6)

```

The below returns true as Y < X < 6:

```lisp

(< 0 y x 6)

```

Other useful numerical functions are below.

```lisp

;; Returns e^3

(exp 3)

;; Returns 4^5

(expt 4 5)

;; Returns log of 8 in the base 2, i.e. 3

(log 8 2)

;; Returns square root of 9, i.e. 3

(sqrt 9)

;; Returns 5

(max 1 3 5 4 2)

;; Returns -1

(min 1 -1 2 3 4)

;; Returns 3

(abs -3)

```

More details on numerical operations can be found in [Common Lisp, the Language 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node121.html).


<a id="org509a49b"></a>

## Text Functions

`CONCATENATE` is a useful function to join strings. Try the following:

```lisp

(concatenate 'string "Hello, " "world" ". Today is good.")

```

`LENGTH` returns the number of characters in a string. The below will return 6:

```lisp

(length "Common")

```

`SEARCH` will search for a substring within a string. The below will return 4, the starting position of the first string within the second string:

```lisp

(search "term" "the term is search within this string")

```

Below are comparison functions for strings. Replace STRING with CHAR in the below to get the equivalent character comparison function.

| Case Sensitive | Case Insensitive    |
|-------------- |------------------- |
| STRING=        | STRING-EQUAL        |
| STRING/=       | STRING-NOT-EQUAL    |
| STRING<        | STRING-LESSP        |
| STRING<=       | STRING-NOT-GREATERP |
| STRING>        | STRING-GREATERP     |
| STRING>=       | STRING-NOT-LESSP    |


<a id="orgbb85d6f"></a>

# Logic & Equality


<a id="org3d5c207"></a>

## Type Predicates

Common Lisp has a type hierarchy and LISP objects can belong to multiple data types. One can test whether an object is of a particular type with `TYPEP`. The below will return true:

```lisp

(typep "My String" 'string)

```

Other type predicate functions include ATOM, NULL, ZEROP, NUMBERP, EVENP, LISTP, ARRAYP, PLUSP, CHARACTERP, ODDP, SYMBOLP, PACKAGEP, MINUSP, STRINGP and ODDP.


<a id="org2ea5239"></a>

## Boolean & Logic

Falsity is represted by `NIL` and all other values represent truth.

The function `AND` returns `NIL` if any of its arguments are false and returns the value of the last argument if all arguments are true. The below will return 5 as all the arguments are true:

```lisp

(and t (+ 1 2) (* 1 5))

```

The function `OR` returns the first argument that is true and `NIL` if no argument is true. The below returns 3, the first non-nil value in the form:

```lisp

(or nil (+ 1 2) (* 1 5))

```


<a id="org95cc4a3"></a>

## Equality

Common Lisp has a few different functions for testing equality of two objects. Generally speaking, you can't go wrong with `EQUAL`

-   `EQ` compares equality of memory addresses and is the fastest test. It is useful to compare symbols quickly and to test whether two cons cells are physically the same object. It should not be used to compare numbers

-   `EQL` is like `EQ` except that it can safely compare numbers for numerical equality and type equality. It is the default equality test in many Common Lisp functions

-   `EQUAL` is a general purpose test that, in addition to being able to safely compare numbers like `EQL`, can safely compare lists on an element by element basis. Lists are not unique and `EQ` and `EQL` will fail to return equality on equivalent lists if they are stored in different memory addresses

-   `EQUALP` is a more liberal version of `EQUAL`. It ignores case distinctions in strings, among other things

-   `=` is the most efficient way to compare numbers, and the only way to compare numbers of disparate types, such as 3 and 3.0. It only accepts numbers


<a id="orgf591e22"></a>

## Blocks

The `PROGN` form allows multiple forms to be evaluated and the value of the last is returned as the value of the `PROGN`. The below will print "Hello" and "World" and return 10:

```lisp

(progn
  (print "Hello")
  (print "World")
  (+ 5 5))

```

The `BLOCK` special operator is similar, but it is named and has a mechanism for out-of-order exit with the `RETURN-FROM` operator.

-   The value of the last expression in a block is returned as the value of the block (unless there is an early exit by `RETURN` or `RETURN-FROM`)
-   All other expressions in the block are thus only useful for their side effects

The below constructs a block named MY-BLOCK and returns 10 as it returns from the block when evaluating the `RETURN-FROM` form:

```lisp

(block my-block
  (print "We see this")
  (return-from my-block 10)
  (print "We will never see this"))

```

The `RETURN` macro returns its argument as the value of an enclosing `BLOCK` named `NIL`. Many Common Lisp operators that take a body of expressions implicitly enclose the body in a `BLOCK` named `NIL` and we can use `RETURN` in these forms:

```lisp

(dolist (i '(1 2 3 5 6 7))
  (if (= i 3)
      (return 10))
  (print i))

```


<a id="org3a0d565"></a>

## Conditionals

The five main conditionals in Common Lisp are `IF`, `WHEN`, `UNLESS`, `COND` and `CASE`.

Example of `IF` form (note there is **no** implicit `PROGN`):

```lisp

(if (equal 5 (+ 1 4))
    (print "This is true")
    (print "This if false"))

```

Example of `WHEN` form (note there is an implicit `PROGN`):

```lisp

(when (equal 5 (+ 1 4))
  (print "Print if statement is true")
  (print "Print this also"))

```

Example of `UNLESS` form (note there is an implicit `PROGN`):

```lisp

(unless (equal 3 (+ 1 4))
  (print "Only print if condition is false")
  (print "Print this also"))

```

Example of `COND` form (multiple ifs, implicit `PROGN`). The form exits on the first true:

```lisp

(cond ((equal 5 3) (print "This will not print"))
      ((equal 5 5) (print "This will print"))
      ((equal 5 5)
       (print "This will not print as the")
       (print "form exited at first true")))

```

Example of a `CASE` form (multiple ifs on one variable, implicit `PROGN`). Cases are literal and not evaluated. The form exits on the first true:

```lisp

(case (read)
  (1 (format t "~% Monday"))
  (2 (format t "~% Tuesday"))
  (3 (format t "~% Wednesday"))
  (4 (format t "~% Thursday"))
  (5 (format t "~% Friday"))
  (6 (format t "~% Saturday"))
  (7 (format t "~% Sunday"))
  (otherwise "Not an integer between 1 and 7"))

```


<a id="orge0a634f"></a>

# Looping


<a id="org436ea7c"></a>

## Basic Looping

`DOLIST` will iterate over the items of a list and execute the loop body for each item of the list.

```lisp

(dolist (my-variable my-list optional-result-form)
  body-form*)

```

Below is an example that will print the numbers 1 to 7 iteratively:

```lisp

(dolist (i '(1 2 3 5 6 7))
  (print i))

```

`DOTIMES` will iterate from 0 to one less than the end number supplied. If an optional result form is supplied, it will be evaluated at the end of the loop.

```lisp

(dotimes (my-variable end-number optional-result-form)
  body-form*)

```

Below is an example that will print the numbers 0 to 4:

```lisp

(dotimes (i 5 T)
  (print i))

```


<a id="org83b693d"></a>

## Advanced Looping

Below is the syntax of the `DO` macro.

```lisp

(do ((var1 init1 step1)
     ...
     (varn initn stepn))
    (end-test result-forms*)
  body-forms*)

```

The below example will return 81 and print 1, 0, 1, 4, 9, 16, 25, 36, 49 and 64 on newlines. During each iteration, LOOP-STEP is increased by one and SQUARE is set to the square of LOOP-STEP:

```lisp

(do ((loop-step 0 (+ loop-step 1))
     (square 1 (* loop-step loop-step)))
    ((= 10 loop-step) square) ; Stop at 10
  (print square)) ; Print square at each step

```

Below are examples of the `LOOP` macro, some from [Peter D. Karp's Guide](http://www.ai.sri.com/pkarp/loop.html). The first will return a list of the doubles of each number in the original list:

```lisp

(defvar my-list-1 '(1 2 3 4 5 6))

;; Returns (2 4 6 8 10 12)

(loop for x in my-list-1
      collect (+ x x))

```

The below will print each of the numbers in the list iteratively:

```lisp

(loop for x in my-list-1
       do (print x))

```

The below will only collect even numbers:

```lisp

(loop for x in my-list-1
      if (evenp x)
      collect x)

```

The below is an example of iterating across two lists, stopping the loop at the end of the shorter list:

```lisp

(defvar my-list-2 '(a b c d e))

(loop for x in my-list-1
      for y in my-list-2
      do (format t "X: ~a, Y: ~a, " x y))

```

We can also do simple loops with a counter:

```lisp

(loop for x from 1 to 5
       do (print x))

```

Below is an example of how we can use the `LOOP` macro to check whether a certain predicate is true at some point within a list:

```lisp

(loop for x in '(abc 2) 
      thereis (numberp x))

```

We can also check whether a predicate is never true within a loop:

```lisp

(loop for x in '(abc 2) 
      never (numberp x))

```

We can also check whether a predicate is always true within a loop:

```lisp

(loop for x in '(abc 2)
	always (numberp x))

```

Below is an example of terminating a loop early:

```lisp

(loop for x from 1
      for y = (* x 10)
      while (< y 100)
      do (print (* x 5))
      collect y)

```

Finally, a few more examples illustrating the versatility of the `LOOP` macro:

```common-lisp

(loop for x in '(a b c d e 1 2 3 4)
      until (numberp x)
      do
      collect (list x 'abc))

(loop for x in '(a b c d e)
      for y from 1
      when (> y 1) do (format t ", ")
      do (format t "~A" x))

(loop for x in '(a b c d e)
    for y from 1
    if (> y 1)
    do (format t ", ~A" x)
    else do (format t "~A" x))

```


<a id="org92b85fc"></a>

# Local Variables & Functions


<a id="org9f76fb8"></a>

## Local Variables

`LET` and `LET*` are special operators that allow us to create local variables that can only be accessed within their closures. `LET` binds its variables in parallel such that you cannot refer to another variable in the `LET` form when setting the value of another. `LET*` binds its variables in sequentially, so that you can refer to the value of any previously bound variables. This is useful when you want to assign names to several intermediate steps in a long computation.

The `LET` form has the following syntax:

```elisp

(let ((var-1 value-1)
      ...
      (var-n value-n))
  body-form*)

```

Below is an example of `LET*` that will return 10:

```lisp

;; Prints 10

(let* ((x 5)
       (y (+ x x)))
  (print y))

```


<a id="org9b010ba"></a>

## Local Functions

Functions named by `DEFUN` are global functions that can be accessed anywhere. We can define local functions `LABELS`, which are only accessible within their context. The syntax of `LABELS` is:

```lisp

(labels ((fn-1 args-1 body-1)
	 ...
	 (fn-n args-n body-n))
  body-form*)

```

Functions defined within `LABELS` take a similar format to a `DEFUN` form. Within the body of the `LABELS` form, function names matching those defined by the `LABELS` refer to the locally defined functions rather than any global functions with the same names.

Below is an example of a `LABELS` form that will return 12, the result of (+ 2 4 6), where 2, 4 and 6 are the results of evaluating the three local functions defined in the form.

```lisp

(labels ((first-function (x) (+ x x))
	 (second-function (y) (* y y))
	 (third-function (z) (first-function z)))
  (+ (first-function 1)
     (second-function 2)
     (third-function 3))) 

```


<a id="org283acf2"></a>

# More on Functions


<a id="orgfceb856"></a>

## Lambda Expressions

Lambda expressions allow us to create unnamed functions. These are useful when writing small functions for certain tasks. Below is an example that returns 101:

```lisp

((lambda (x)
   (+ x 100))
 1)

```


<a id="org7a4f5d3"></a>

## Function Parameters

By default, a function call must supply values for all parameters that feature in the function definition. We can modify this behaviour with the `&optional`, `&key` and `&rest` tokens.

The `&optional` token allows us to distinguish between required parameters, placed before the `&optional` token, and optional parameters, placed after the token:

```lisp

(defun make-a-list (a b c d &optional e f g)
  (list a b c d e f g))


;; Returns (1 2 3 4 5 NIL NIL)

(make-a-list 1 2 3 4 5)

```

One drawback of the `&optional` token, using the above as an example, is that we need to supply values for E and F if we want to supply the value for G, as arguments in a function call are assigned to the parameters in order.

To overcome this, we utilise the `&key` token to be able to specify which optional parameter we want to assign a value to. Below is an example of this.

```lisp

(defun make-a-list-2 (a b c d &key (e 1) f g)
  (list a b c d e f g))


;; Returns (1 2 3 4 1 NIL 7)

(make-a-list-2 1 2 3 4 :g 7)

```

In general, `&key` is preferable to `&optional` as it allows us to have greater control in our function calls. It also makes code easier to maintain and evolve as we can add new parameters to a function without affecting existing function calls (useful when writing libraries that are already dependencies for other programs).

Finally, the `&rest` token, placed before the last variable in a parameter list, allows us to write functions that can accept an unknown number of arguments. The last variable will be set to a list of all the remaining arguments supplied by the function call:

```lisp

(defun make-a-list-3 (a b c d &rest e) (list a b c d e))

(make-a-list-3 1 2 3 4 5 6 7 8) ; (1 2 3 4 (5 6 7 8))

```

We can utilise multiple tokens in the same function call, as long as we declare them in order:

1.  First the names of required parameters are declared
2.  Then the `&optional` parameters
3.  Then the `&rest` parameter
4.  Finally the `&keyword` parameters are declared


<a id="org86b4d87"></a>

## Multiple Values

The `VALUES` function returns multiple values and can be used as the last expression in the body of a function. The below example returns 1, NIL and 6 (individually, not as a list):

```lisp

(values 1 nil (+ 2 4))

```

If a `VALUES` function is supplied as an argument to a form which is only expecting one value, the first value returned by the `VALUES` function is used and the rest are discarded:

```lisp

;; Returns 6

(+ 5 (values 1 nil (+ 2 4)))

```

The `MULTIPLE-VALUE-BIND` macro is used to receive multiple values. The first argument of this macro is a list of parameters and the second is an expression whose values are bound to the parameters. We can then use these bindings in the body of the `MULTIPLE-VALUE-BIND` macro.

Below is an example that returns (1 2 3):

```lisp

(multiple-value-bind (x y z) (values 1 2 3)
   (list x y z)) 

```

If there are more variables than values, the leftover variables will be bound to NIL. If there are more values than variables, the extra values will be discarded.


<a id="orgc982106"></a>

## Apply & Funcall

Functions in LISP are first-class objects that generally support all operations available to other data objects, such as being modified, passed as an argument, returned from a function and being assigned to a variable.

The `FUNCTION` special operator (shorthand `#'`) returns the function object associated with the name of function that is supplied as an argument:

```lisp

(function +)

;; Equivalent syntax

#'+

```

`APPLY` takes a function and a list of arguments for it and returns the result of applying the function to its arguments.

Below is an example that returns 6. Note how we have to use to sharp-quote `#'` to pass the `+` function as an object into the `APPLY` function. Without doing so, LISP will return an error as it will try to evaluate `+`, which is not legally permissible in the below example.

```lisp

(apply #'+ '(1 2 3))

```

The below is an example that illustrates the use of lambda expressions:

```lisp

(apply #'(lambda (a b c)
	   (+ a b c))
       '(1 2 3))

```

The function `FUNCALL` is similar to `APPLY`, but allows us to pass arguments individually and not as a list. Below is an example that returns 6:

```lisp

(funcall #'+ 1 2 3)

```


<a id="orgc1e2096"></a>

## Mapping Functions

Mapping is a type of iteration in which a function is successively applied to pieces of one or more sequences.

`MAPCAR` operates on successive elements of lists and returns a list of the result of the successive calls to the function specified. The below example will return (-1 -2 -3):

```lisp

(mapcar #'(lambda (x) (- 0 x)) '(1 2 3))

```

`MAPLIST` operates on successive CDRs of the lists. The below example will return ((A B C D) (B C D) (C D) (D)):

```lisp

(maplist #'(lambda (x) x) '(a b c d))

```

The above only work for lists. To map over other types of sequences, one can use `MAP`:

```lisp

(map result-type function &rest sequences)

```

Below are a couple of examples.

```lisp

;; Returns a list ((#\a #\a) (#\b #\b) (#\c #\c))

(map 'list #'(lambda (x) (list x x)) "abc")

;; Returns "1010"

(map 'string
     #'(lambda (x) (if (oddp x) #\1 #\0))
     '(1 2 3 4))


```


<a id="org721d229"></a>

# More on Lists


<a id="org681ae0f"></a>

## List Functions

`NTH` can be used to access a particular element of a list. The below returns D, the fourth element of the list:

```lisp

(nth 3 '(a b c d e f g))

```

`NTHCDR` is a similar version to access a particular CDR of a list. The below returns (D E F G):

```lisp

(nthcdr 3 '(a b c d e f g))

```

We can also use `FIRST`, `SECOND`, &#x2026;, `TENTH` to access the first ten elements of a list. The below returns A:

```lisp

(first '(a b c d e f g))

```

`LAST` is a very useful function to access the last CDR of a list. The below returns (G):

```lisp

(last '(a b c d e f g)) 

```

Lists can be thought of as sets and a number of useful functions exist to perform set operations.

`MEMBER` and its variants are useful functions to test whether certain elements exist within a list. The below returns (B C), the CDR from the position B is found:

```lisp

(member 'b '(a b c))

```

The below returns (3 4), the odd numbers in the list:

```lisp

(member-if #'oddp '(2 3 4))

```

We can also specify the test to apply (the default is `EQL`):

```lisp

(member 'b '(a b c) :test #'equal)

```

`ADJOIN` joins an object onto a list only if it is not already a member. The below returns (A B C) as B is already a member of the supplied list (A B C):

```lisp

(adjoin 'b '(a b c))

```

The below returns (Z A B C) as Z is not a member of the supplied list:

```lisp

(adjoin 'z '(a b c))

```

Set union, intersection and complement operations can be done with `UNION`, `INTERSECTION` and `SET-DIFFERENCE`.

The below example returns (A B C S), the union of the two lists supplied:

```lisp

(union '(a b c) '(c b s))

```

The below examples returns (C B), the interesection of the two lists supplied:

```lisp

(intersection '(a b c) '(c b s))

```

The below returns (A), a list of the elements in the first list supplied that do not exist in the second list:

```lisp

(set-difference '(a b c) '(c b s))

```

Finally, the function `REDUCE` is useful to extend functions that only take two variables. It takes two arguments, a function (which must take exactly two values) and a sequence.

-   The function is initially called on the first two elements of the sequence, and thereafter with each successive element as the second argument

-   The value returned by the last call is the value returned by the `REDUCE` function

For example, the below returns (A), the intersection of these three lists:

```lisp

(reduce #'intersection '((b r a d) (b a d) (c a t)))

```


<a id="orgc1be892"></a>

## Push, Pop & Reverse

The macro `PUSH` can be used to push an element to the front of a list, while the macro `POP` can remove and return the first element of the list. Both are destructive operations as they directly change the original lists in question.

The below is an example of pushing 1 to the front of \*​MY-LIST\*:

```lisp

(defparameter *my-list* '(2 3 4))

(push 1 *my-list*)

;; Returns (1 2 3 4)

*my-list*

```

The below is an example that returns the first item of \*​MY-LIST\* and removes it from the list as well:

```lisp

;; Returns 1, the car of *my-list*

(pop *my-list*)

;; Returns (2 3 4)

*my-list*

```

`REVERSE` is a very useful function to reverse the order of elements within a list and is frequently used in various scenarios.

Below is an example that returns (F E D C B A):

```lisp

(reverse '(a b c d e f))

```


<a id="org34d47a7"></a>

## Association Lists

Association lists are a very useful data structure for mapping values to keys. They are lists of pairs (i.e. conses), with the key being the `CAR` of the pair and the datum being the `CDR` of the pair.

Below is an example of an association list constructed using dot pair notation:

```lisp

(defvar my-a-list '((one . 1) (two . 2)))

```

Below is an example of returning a new association list by adding an entry to the front of a pre-existing association list:

```lisp

;; Returns ((THREE . 3) (ONE . 1) (TWO . 2))

(acons 'three 3 my-a-list)

```

Below is an example of creating an association list from lists of keys & datums:

```lisp

(pairlis '(one two three) '(1 2 3))

```

We can use `ASSOC` to retrieve the pair associated with a key. The below returns (ONE . 1):

```lisp

(assoc 'one my-a-list)

```

We can use `RASSOC` to retrieve the pair associated with a datum. The below returns (TWO . 2):

```lisp

(rassoc 2 my-a-list :test #'=)

```


<a id="org4faae7d"></a>

# More on Sequences

Sequences are a data type in Lisp and lists, strings and arrays are all of type sequence.


<a id="org6377bc9"></a>

## Arrays

The function `MAKE-ARRAY` creates arrays. For example, we can create a 2 x 3 array as follows:

```lisp

(defparameter my-array
  (make-array '(2 3) :initial-element nil))

```

We can use `AREF` and `SETF` to access elements and set their values. The below will return NIL as the value of this element has not been set yet:

```lisp

(aref my-array 0 0)

```

The below will set the value of this element to B, which will be the value returned by the second form below:

```lisp

(setf (aref my-array 0 0) 'b)

(aref my-array 0 0)

```

We use `:INITIAL-ELEMENT` to set the value of every element of an array to the provided argument. In the below, every element has a value of 2.

```lisp

(setf my-array
       (make-array '(2 3)
		   :initial-element '((1 2 3) (1 2 3))))

```

The functions `ARRAY-RANK` and `ARRAY-DIMENSION` retrieve the the number of dimensions and the number of elements in a given dimension respectively:

```lisp

;; Returns 2

(array-rank my-array))

;; Returns 2

(array-dimension my-array 0)

;; Returns 3

(array-dimension my-array 1)

```

`:INITIAL-CONTENTS` is used to set the array to an object provided.

```lisp

(defparameter my-vector
  (make-array 3 :initial-contents '("a" 'b 3)))

```

A one-dimensional array is also known as a vector and the above example created one. Vectors can also be created with `VECTOR`:

```lisp

(vector "a" 'b 3)

```

The most famous vectors in LISP are strings. Strings are specialised vectors whose elements are characters.


<a id="org469c85e"></a>

## Sequence Functions

Sequences have many useful functions. We can use `LENGTH` to return the number of items in a sequence. The below returns 6:

```lisp

(length '(a b c d e f))

```

`REMOVE` and its variants are very handy filter functions. The below returns (C R T) as a new list:

```lisp

(remove 'a '(c a r a t))

```

The below returns "cdbra", preserving only the last duplicate of each item:

```lisp

(remove-duplicates "abracadabra")

```

The below returns (2 4 4) by removing all odd numbers:

```lisp

(remove-if #'oddp '(1 2 3 4 4))

```

`SUBSEQ` can extract a portion of a sequence. Its arguments are a list, the starting position and an optional ending position (which is not to be included in the subsequence). The below will return (B C D):

```lisp

(subseq '(a b c d e f) 1 4)

```

`SORT` takes a sequence and a comparison function of two arguments and destructively (i.e. by modifying the original sequence) returns a sequence sorted according to the function. The below returns (6 5 4 2 1) by sorting in descending order:

```lisp

(sort '(1 4 2 5 6) #'>)

```

The functions `EVERY` and `SOME` test whether a sequence satisfies a provided predicate. The below will return NIL as not every item is odd:

```lisp

(every #'oddp '( 1 2 5))

```

The below will return T as some of the items are odd:

```lisp

(some #'oddp '( 1 2 5))

```

The below will return T as the items of the first sequence are greater than those of the second sequence in an element-wise comparison:

```lisp

(every #'> '(1 3 5) '(0 2 4))

```

We can find elements within a sequence with `FIND`, which returns the leftmost such element, or `POSITION`, which returns the position of such an item, as an integer. The below returns 1, the item we are searching for:

```lisp

(find 1 '(1 2 3 4))

```

The below returns 0, the position of the item we are searching for:

```lisp

(position 1 '(1 2 3 4))

```

We can use `COUNT` to count the number of instances of the element within the sequence. The below returns 3, the number of instances of 1 in the list:

```lisp

(count 1 '(1 2 3 1 1 4))

```

We can use `SEARCH` to search for sequence within another. The below returns 4, the position of the string "Hello" in "Hi! Hello, World!":

```lisp

(search "Hello" "Hi! Hello, World!")

```


<a id="org5e7bf39"></a>

## Keyword Arguments

Many list and sequence functions take one or more keyword arguments from the below table. For example, we can use `POSITION` to return the position of an element within a sequenc (or NIL if not found) and use keyword arguments to determine where to begin the search.

The below will return 4:

```lisp

(position #\a "fantasia" :start 3 :end 7)

```

| Parameter | Position                            | Default  |
|--------- |----------------------------------- |-------- |
| :key      | A function to apply to each element | identity |
| :test     | The test function for comparison    | eql      |
| :from-end | If true, work backwards             | nil      |
| :start    | Position at which to start          | 0        |
| :end      | Position, if any, at which to stop  | nil      |


<a id="org94ffa30"></a>

# Data Structures


<a id="orgb7bdb2a"></a>

## Hash Tables

A hash table is a way of associating pairs of objects, like a dictionary. The objects stored in a hash table or used as keys can be of any type. We can make hastables with `MAKE-HASH-TABLE` and retrieve values associated with a given key with `GETHASH`:

```lisp

(defparameter my-hash-table (make-hash-table))

;; Returns NIL as not yet set

(gethash 'color my-hash-table) 

```

Similar to other structures, we use `SETF` to set values. Hash tables can accommodate any number of elements, because they are expanded when they run out of space.

```lisp

(setf (gethash 'color my-hash-table) 'red)  

```

The function `MAPHASH` allows you to iterate over all entries in the hash table.

-   Its first argument must be a function which accepts two arguments, the key and the value of each entry
-   Note that due to the nature of hash tables you can't control the order in which the entries are provided to `MAPHASH` (or other traversing constructs)

Below is an example, which will return COLOR = RED as there is only one item (COLOR) currently in our hash table.

```lisp

(maphash #'(lambda (key value)
	     (format t "~A = ~A~%" key value))
	 my-hash-table)

```

You can remove items from a hash table with `REMHASH`:

```lisp

(remhash 'color my-hash-table)

```


<a id="orge20b38c"></a>

## Structures

Common Lisp provides the `DEFSTRUCT` facility for creating data structures with named components. This makes it easier to manipulate custom data objects as we can refer to their components by name.

Constructor, access and assignment constructs are automatically defined when a data type is defined through `DEFSTRUCT`. Consider the below example of defining a data type for rectangles.

`DEFSTRUCT` defines RECTANGLE to be a structure with two fields, HEIGHT and WIDTH. The symbol RECTANGLE becomes the name of a data type and each rectangle will be of type RECTANGLE, then STRUCTURE, then ATOM and then T.

`DEFSTRUCT` will generate four associated functions:

1.  RECTANGLE-HEIGHT and RECTANGLE-WIDTH to access elements of the structure

2.  RECTANGLE-P to test whether an object is of type RECTANGLE

3.  MAKE-RECTANGLE to create rectangles

4.  COPY-RECTANGLE to create copies of rectangles

Below is an example of the above structure.

```lisp

;; Height will default to NIL while width will default to 5 

(defstruct rectangle
  (height)
  (width 5))

```

The below creates an instance of RECTANGLE:

```lisp

(defvar rectangle-1)

(setf rectangle-1
      (make-rectangle :height 10 :width 15))

```

The below will return 10:

```lisp

(rectangle-height rectangle-1)

```

The below will set RECTANGLE-WIDTH of RECTANGLE-1 to 20:

```lisp

(setf (rectangle-width rectangle-1) 20)

```


<a id="orgab67257"></a>

## Common Lisp Object System (CLOS)

Below is an example of creating two classes, one which inherits from the other. Courtesy of the [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/clos.html).

```lisp

;; Define a class:

(defclass person ()
 ((name
  :initarg :name
  :accessor name)
 (lisper
  :initform "Yes"
  :accessor lisper)))

;; Create an instance of this class:

(defvar person-1
  (make-instance 'person :name "David" ))

```

Accessor functions are used for both getting and setting. The below will return DAVID.

```lisp

(name person-1)

```

The below will set name to TOM.

```lisp

(setf (name person-1) "Tom")

```

`:INITFORM` is used to set default values. The below returns "Yes" (the default value) as the value of LISPER was not yet set.

```lisp

(lisper person-1)

```

The below is an example of inheriting from the PERSON class.

```lisp

(defclass child (person)
  ((can-walk-p
   :initarg :can-walk-p
   :initform "No"
   :accessor can-walk-p)))

```

Inherited classes inherit the slots of their parents. CHILD will inherit LISPER from PERSON. The below will return "Yes":

```lisp

(lisper (make-instance 'child :name "Phoebe"))

```

Inherited classes can also introduce new slots. CHILD introduces CAN-WLAK-P. The below will return "No":

```lisp

(can-walk-p (make-instance 'child))

```

We can add methods to classes with a combination of `DEFGENERIC` and `DEFMETHOD`. Note that Common Lisp supports multiple dispatch so that many classes can share and use the same method names.

`DEFGENERIC` establishes an entry in the method dispatch table, while `DEFMETHOD` allows us to create specialised versions.

```lisp

;; Version with a default method (to be used if no other specialisations exist:

(defgeneric greet (obj)
  (:documentation "Says hi")
  (:method (obj)
	   (format t "Hi")))

;; Version without default method:

(defgeneric greet-2 (obj1 obj2)
  (:documentation "Says hi"))

```

In creating specialised methods, we add the parameter type to the parameter list. In a method call, LISP will then use the method which matches the parameter types of the parameters supplied in the method call.

In the below, GUEST-NAME is a parameter of type PERSON, while MESSAGE is a parameter that is not specialised and can be anything.

```lisp

(defmethod greet-2 ((guest-name person) message)
  (format t "The person greets ~A and says ~A" guest-name message))

```

Finally, it is useful to create custom print output for CLOS objects. This can be achieved with the following.

```lisp

  (defmethod print-object ((obj person) stream)
  (print-unreadable-object (obj stream :type t)
			   (format stream "~a" (name obj))))

;; Returns #<Person Tom>

  (print person-1)

```


<a id="orge334e2e"></a>

# Other


<a id="org5926aaf"></a>

## Reading & Writing to Files

The `WITH-OPEN-FILE` macro is used to read and write to files. Below is an example of opening a file and then reading from it. The `NIL` in the below inhibits end of file errors.

```lisp

(with-open-file (my-stream "/Users/ashokkhanna/test.txt")
  (format t "~a~%" (read-line my-stream nil)))

```

Below is an example of opening a file and then writing to it.

```lisp

(with-open-file (my-stream "/Users/ashokkhanna/test.txt" :direction
			   :output :if-exists :append)
  (format my-stream "~a~%" "Hello, World!"))

```

The following open arguments can be supplied to the `WITH-OPEN-FILE` macro:

| Arguments                  | Effect                             |
|-------------------------- |---------------------------------- |
| :direction :output         | Write to a file insead of reading  |
| :if-does-not-exist :create | Create a file if it does not exist |
| :if-exists :supersede      | Replace the file that exists       |
| :if-exists :overwrite      | Overwrite file                     |
| :if-exists :append         | Write to end of file               |


<a id="orgc750fcf"></a>

## Packages

Packages are a central mechanism for avoiding name collisions that occur if multiple files contain variables or functions with the same name. More information on packages can be found on [my guide on Medium](https://ashok-khanna.medium.com/an-introduction-to-lisp-packages-7a9ee352006e).

Packages need to be registered before they can be used. Below is an example of registering a package that inherits from two packages and exports two symbols. This example also shadows two symbols, allowing us to use, within MY-PACKAGE, the definitions of these symbols (RESTART and CONDITION in our case) that exist within the package MY-PACKAGE and not definitions of these symbols inherited from other packages (CL in our case, where RESTART and CONDITION are interned also).

```lisp

(defpackage :my-package
  (:use :cl :other-package-1)
  (:export :symbol-1
	   :restart)
  (:shadow :restart
	   :condition))

```

Once a package is registered with the above, we can switch to it with `IN-PACKAGE`.

-   **Within a package**, all symbols defined in that package are accessible. In addition, any exported symbols from packages inherited via the `:USE` command can be directly accessed without a package qualifier

-   **Outside of a package**, internal symbols can be accessed via a double-colon package qualifier, e.g. `my-package::symbol-3`, while exported symbols can be accessed via a single-colon package qualifier, e.g. `my-package:symbol-1`

It is good practice to put the above at the top of LISP files so that readers can easily follow which package is currently live in a file.

```lisp

(in-package :my-package)

```

Finally, note in the above, that we will get an error if we try to inherit from both CL and MY-PACKAGE due to to the clash in symbols RESTART and CONDITION that appear in both packages. To overcome this, we can use a `:shading-import-from:` command, such as in the following.

```lisp

(defpackage :my-package-2
  (:use :cl :my-package)
  (:shadowing-import-from :my-package :restart))

```
