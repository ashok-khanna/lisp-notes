
# Table of Contents

1.  [The Basics](#orgc00fd2d)
    1.  [Global Variables](#org1fc0ad2)
    2.  [Global Functions](#orgd0b0a3b)
    3.  [Assignment](#orga173c97)
    4.  [Input & Output](#org35ab6e9)
    5.  [Numerical Functions](#org4406857)
    6.  [Text Functions](#org42597b0)
2.  [Logic & Equality](#orgce131d1)
    1.  [Type Predicates](#org2898914)
    2.  [Boolean & Logic](#org53aabbc)
    3.  [Equality](#org40b3bb0)
    4.  [Blocks](#org42fe25f)
    5.  [Conditionals](#org241db32)
3.  [Looping](#orge7dfe35)
    1.  [Basic Looping](#orgc3abd4d)
    2.  [Advanced Looping](#orgf4de905)
4.  [Local Variables & Functions](#org167f808)
    1.  [Local Variables](#orgb9aef03)
    2.  [Local Functions](#orga4be39c)
5.  [More on Functions](#orgaf6eef4)
    1.  [Lambda Expressions](#org4075850)
    2.  [Function Parameters](#org24845ae)
    3.  [Multiple Values](#org8bfb896)
    4.  [Apply & Funcall](#org54f0191)
    5.  [Mapping Functions](#org23997f8)
6.  [More on Lists](#orgd4ca0b8)
    1.  [List Functions](#org5c772ed)
    2.  [Push, Pop & Reverse](#org4bf54b9)
    3.  [Association Lists](#org76442d1)
7.  [More on Sequences](#orgbe1388e)
    1.  [Arrays](#org4ad89be)
    2.  [Sequence Functions](#org87d5a34)
    3.  [Keyword Arguments](#org6ce7715)
8.  [Data Structures](#org15a50bd)
    1.  [Hash Tables](#orgf297da4)
    2.  [Structures](#orgb12745c)
    3.  [Common Lisp Object System (CLOS)](#org558a0c8)
9.  [Other](#org4bfaf56)
    1.  [Reading & Writing to Files](#org983e82d)
    2.  [Packages](#org4060968)


<a id="orgc00fd2d"></a>

# The Basics


<a id="org1fc0ad2"></a>

## Global Variables

We can define global variables with `DEFPARAMETER` and `DEFVAR`. `DEFPARAMETER` will always bind the supplied value, while `DEFVAR` will only bind a value to a variable if no binding exists.

    
    ;; Establish an unbound variable
    
    (defvar *x*)
    
    ;; Assign the value 15 to X
    
    (defparameter *x* 15)
    
    ;; Does nothing as X already bound
    
    (defvar *x* 10)

We can define global constants with `DEFCONSTANT`:

    
    (defconstant +my-constant+ 20)


<a id="orgd0b0a3b"></a>

## Global Functions

Global functions are defined with `DEFUN` using the below syntax.

    
    (defun function-name (parameter*)
      "Optional documentation string."
      body-form*)

Below is example of a function that multiplies the sum of two numbers by 10.

    
    (defun multiply-sum-by-10 (x y)
      "Returns the sum of two numbers multiplied by 10"
      (* 10 (+ x y)))
    
    ;; Returns 150
    
    (multiply-sum-by-10 5 10)


<a id="orga173c97"></a>

## Assignment

`SETF` is LISP's general purpose assignment operator. It is a powerful setter that can assign values to many different types of places.

The syntax of `SETF` is as follows.  Places are typically either symbols, representing variables or functions, or getter forms that access particular places in objects that we wish to modify.

    
    (setf place value)

Below are some examples.

    
    ;; Set x to 10
    
    (setf x 10)
    
    ;; Set x to 1 and y to 2
    
    (setf x 1 y 2)
    
    ;; Example of using a getter with setf
    
    (defvar *list* '(1 2 3 4))
    
    (setf (car *list*) 5)
    
    ;; Returns (5 2 3 4)
    
    *list*


<a id="org35ab6e9"></a>

## Input & Output

The LISP reader consists of three primary functions

-   The function `READ` is used to parse input into Lisp objects and reads exactly one expression, regardless of newlines

-   The function `READ-LINE` reads all characters up to a newline, returning a string

-   The function `READ-FROM-STRING` takes a string and returns the first
    expression from it

Below are some examples you can experiment with.

    
    (defparameter my-variable nil)
    
    (setf my-variable (read-line))
    
    (setf my-variable (read))
    
    (setf my-variable (read-from-string "(1 2 3)"))

The most useful LISP printing functions are:

-   `PRINT` and `PRIN1` are used to generate output useful for programs (the former adds a newline to the output whilst the latter does not)

-   `PRINC` is used for output for people

-   `FORMAT` is a highly configurable printer and the most commonly used printer

The syntax of `FORMAT` is as follows:

    
    (format destination control-string optional-arguments*)

The first argument of the `FORMAT` function is the destination where the output will be printed.

-   A value of `T` will send the out to the stream `*​STANDARD-OUTPUT​*` (typically the main screen of your LISP system) whilst `NIL` here will return the output as a string. We can also supply a stream here
-   The second argument is the string that we want to print. However, we can enter directives (preceded by `~`) to add complex behaviour to the string, such as
    -   `~%` to print newlines
    -   `~A` to print LISP objects for humans
    -   `~S` to print LISP objects for computers, i.e. suitable as as input for the LISP reader

The third (optional) argument of the `FORMAT` function is the arguments we want to supply to the control string. Each `~A` or `~S` in the control-string takes a successive argument from here and places it into the string.

This is best illustrated by the following examples. Note how Bob is quoted in the second example, as the printed representation of LISP strings includes quotes.

    
    ;; Prints to screen: Dear Bob, How are you?
    
    (format t "Dear ~A, ~% How are you?" "Bob")
    
    ;; Prints to screen: Dear "Bob", How are you?
    
    (format t "Dear ~S, How are you?" "Bob")
    
    ;; Returns "Number is: 3" (a string)
    
    (format nil "~A ~A" "Number is:" (+ 1 2))


<a id="org4406857"></a>

## Numerical Functions

Basic numerical functions include `+`, `*`, `-`, `/`. They can take more than two operands.

    
    ;; Returns 20
    
    (+ 4 7 9)

Numerical comparisons can be achieved with `=`, `/=` (not equal to), `>`, `<`, `>=` and `<=`. With three or more arguments, these functions act as range checks.

For example, the below returns true as X is between 0 and 5 inclusive.

    
    (defparameter x 5)
    
    (<= 0 x 5)

The below returns false as X > Y:

    
    (defparameter y 4)
    
    (< 0 x y 6)

The below returns true as Y < X < 6:

    
    (< 0 y x 6)

Other useful numerical functions are below. 

    
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

More details on numerical operations can be found in [Common Lisp, the Language 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node121.html).


<a id="org42597b0"></a>

## Text Functions

`CONCATENATE` is a useful function to join strings. Try the following:

    
    (concatenate 'string "Hello, " "world" ". Today is good.")

`LENGTH` returns the number of characters in a string. The below will return 6:

    
    (length "Common")

`SEARCH` will search for a substring within a string. The below will return 4, the starting position of the first string within the second string:

    
    (search "term" "the term is search within this string")

Below are comparison functions for strings. Replace STRING with CHAR in the below to get the equivalent character comparison function. 

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Case Sensitive</th>
<th scope="col" class="org-left">Case Insensitive</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">STRING=</td>
<td class="org-left">STRING-EQUAL</td>
</tr>


<tr>
<td class="org-left">STRING/=</td>
<td class="org-left">STRING-NOT-EQUAL</td>
</tr>


<tr>
<td class="org-left">STRING<</td>
<td class="org-left">STRING-LESSP</td>
</tr>


<tr>
<td class="org-left">STRING<=</td>
<td class="org-left">STRING-NOT-GREATERP</td>
</tr>


<tr>
<td class="org-left">STRING></td>
<td class="org-left">STRING-GREATERP</td>
</tr>


<tr>
<td class="org-left">STRING>=</td>
<td class="org-left">STRING-NOT-LESSP</td>
</tr>
</tbody>
</table>


<a id="orgce131d1"></a>

# Logic & Equality


<a id="org2898914"></a>

## Type Predicates

Common Lisp has a type hierarchy and LISP objects can belong to multiple data types.
One can test whether an object is of a particular type with `TYPEP`. The below will return true:

    
    (typep "My String" 'string)

Other type predicate functions include ATOM, NULL, ZEROP, NUMBERP, EVENP, LISTP, ARRAYP, PLUSP, CHARACTERP, ODDP, SYMBOLP, PACKAGEP, MINUSP, STRINGP and ODDP.


<a id="org53aabbc"></a>

## Boolean & Logic

Falsity is represted by `NIL` and all other values represent truth.

The function `AND` returns `NIL` if any of its arguments are false and returns the value of the last argument if all arguments are true. The below will return 5 as all the arguments are true:

    
    (and t (+ 1 2) (* 1 5))

The function `OR` returns the first argument that is true and `NIL` if no argument is true. The below returns 3, the first non-nil value in the form:

    
    (or nil (+ 1 2) (* 1 5))


<a id="org40b3bb0"></a>

## Equality

Common Lisp has a few different functions for testing equality of two objects. Generally speaking, you can't go wrong with `EQUAL`

-   `EQ` compares equality of memory addresses and is the fastest test. It is useful to compare symbols quickly and to test whether two cons cells are physically the same object. It should not be used to compare numbers

-   `EQL` is like `EQ` except that it can safely compare numbers for numerical equality and type equality. It is the default equality test in many Common Lisp functions

-   `EQUAL` is a general purpose test that, in addition to being able to safely compare numbers like `EQL`, can safely compare lists on an element by element basis. Lists are not unique and `EQ` and `EQL` will fail to return equality on equivalent lists if they are stored in different memory addresses

-   `EQUALP` is a more liberal version of `EQUAL`. It ignores case distinctions in strings, among other things

-   `=` is the most efficient way to compare numbers, and the only way to compare numbers of disparate types, such as 3 and 3.0. It only accepts numbers


<a id="org42fe25f"></a>

## Blocks

The `PROGN` form allows multiple forms to be evaluated and the value of the last is returned as the value of the `PROGN`. The below will print "Hello" and "World" and return 10:

    
    (progn
      (print "Hello")
      (print "World")
      (+ 5 5))

The `BLOCK` special operator is similar, but it is named and has a mechanism for out-of-order exit with the `RETURN-FROM` operator.

-   The value of the last expression in a block is returned as the value of the block (unless there is an early exit by `RETURN` or `RETURN-FROM`)
-   All other expressions in the block are thus only useful for their side effects

The below constructs a block named MY-BLOCK and returns 10 as it returns from the block when evaluating the `RETURN-FROM` form:

    
    (block my-block
      (print "We see this")
      (return-from my-block 10)
      (print "We will never see this"))

The `RETURN` macro returns its argument as the value of an enclosing `BLOCK` named `NIL`.  Many Common Lisp operators that take a body of expressions implicitly enclose the body in a `BLOCK` named `NIL` and we can use `RETURN` in these forms:

    
    (dolist (i '(1 2 3 5 6 7))
      (if (= i 3)
          (return 10))
      (print i))


<a id="org241db32"></a>

## Conditionals

The five main conditionals in Common Lisp are `IF`, `WHEN`, `UNLESS`, `COND` and `CASE`.

Example of `IF` form (note there is **no** implicit `PROGN`):

    
    (if (equal 5 (+ 1 4))
        (print "This is true")
        (print "This if false"))

Example of `WHEN` form (note there is an implicit `PROGN`):

    
    (when (equal 5 (+ 1 4))
      (print "Print if statement is true")
      (print "Print this also"))

Example of `UNLESS` form (note there is an implicit `PROGN`):

    
    (unless (equal 3 (+ 1 4))
      (print "Only print if condition is false")
      (print "Print this also"))

Example of `COND` form (multiple ifs, implicit `PROGN`). The form exits on the first true:

    
    (cond ((equal 5 3) (print "This will not print"))
          ((equal 5 5) (print "This will print"))
          ((equal 5 5)
           (print "This will not print as the")
           (print "form exited at first true")))

Example of a `CASE` form (multiple ifs on one variable, implicit `PROGN`). Cases are literal and not evaluated. The form exits on the first true:

    
    ;; Try entering in 9 and then (* 3 3) at the read prompt. Then try entering 0
    
    (case (read)
       ((1 3 5 7 9 (* 3 3)) "Odd")
      (* 3 3)
      (0 ; Note implicit PROGN here
       (print "Zero")
       (print "Number"))
      (otherwise "Not a odd number < 10"))


<a id="orge7dfe35"></a>

# Looping


<a id="orgc3abd4d"></a>

## Basic Looping

`DOLIST` will iterate over the items of a list and execute the loop body for each item of the list.

    
    (dolist (my-variable my-list optional-result-form)
      body-form*)

Below is an example that will print the numbers 1 to 7 iteratively.

    
    (dolist (i '(1 2 3 5 6 7))
      (print i))

`DOTIMES` will iterate my-variable from 0 to one less than the end-number supplied. If an optional-result-form is supplied, it will be evaluated at the end of the loop. 

    
    (dotimes (my-variable end-number optional-result-form)
      body-form*)

Below is an example which will print the numbers 0 to 4:

    
    (dotimes (i 5 T)
      (print i))


<a id="orgf4de905"></a>

## Advanced Looping

Below is the syntax and example of the `DO` macro. 

    
    (do ((var1 init1 step1)
         ...
         (varn initn stepn))
        (end-test result-forms*)
      body-forms*)

The below example will return 81 and print 1, 0, 1, 4, 9, 16, 25, 36, 49 and 64 on newlines. During each iteration, loop-step is increased by one while square is set to the square of loop-step.

    
    (do ((loop-step 0 (+ loop-step 1))
         (square 1 (* loop-step loop-step)))
        ((= 10 loop-step) square) ; Stop at 10
      (print square)) ; Print square at each step

Below are examples of the `LOOP` macro, some from [Peter D. Karp's Guide](http://www.ai.sri.com/pkarp/loop.html).

    
      (defvar my-list-1 '(1 2 3 4 5 6))
    
      (defvar my-list-2 '(a b c d e))
    
      ;; Returns (2 4 6 8 10 12)
    
      (loop for x in my-list-1
    	collect (+ x x))
    
      ;; Prints the numbers iteratively:
    
      (loop for x in my-list-1
    	do (print x))
    
      ;; Only collects even numbers:
    
      (loop for x in my-list-1
    	if (evenp x)
    	collect x)
    
      ;; Loops across two lists, but stops
      ;; at the end of the shorter list
    
      (loop for x in my-list-1
    	for y in my-list-2
    	do (format t "X: ~a, Y: ~a" x y))
    
      ;; Simple loop with counter:
    
      (loop for x from 1 to 5
    	do (print x))
    
      ;; Returns True:
    
      (loop for x in '(abc 2) 
          thereis (numberp x))
    
        ;; Returns NIL:
    
    (loop for x in '(abc 2) 
          never (numberp x))
    
      ;; Returns NIL:
    
      (loop for x in '(abc 2)
    	always (numberp x))
    
      ;; Early termination example:
    
      (loop for x from 1
          for y = (* x 10)
          while (< y 100)
          do (print (* x 5))
          collect y)
    
      ;; Some other interesting examples:
    
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


<a id="org167f808"></a>

# Local Variables & Functions


<a id="orgb9aef03"></a>

## Local Variables

`LET` and `LET*` are special operators that allow us to create local variables that can only be accessed within their closures. `LET` binds its variables in parallel such that you cannot refer to another variable in the `LET` form when setting the value of another. `LET*` binds its variables in sequentially, so that you can refer to the value of any previously bound variables. This is useful when you want to assign names to several intermediate steps in a long computation.

The `LET` form has the following syntax:

    
    (let ((var-1 value-1)
          ...
          (var-n value-n))
      body-form*)

An example of `LET*` in use:

    
    ;; Prints 10
    
    (let* ((x 5)
           (y (+ x x)))
      (print y))


<a id="orga4be39c"></a>

## Local Functions

Functions named by `DEFUN` are global functions that can be accessed anywhere. We can define local functions `LABELS`, which are only accessible within their context. The syntax of `LABELS` is:

    
    (labels ((fn-1 args-1 body-1)
    	 ...
    	 (fn-n args-n body-n))
      body-form*)

Functions defined within `LABELS` take a similar format to a `DEFUN` form. Within the body of the `LABELS` form, function names matching those defined by the `LABELS` refer to the locally defined functions rather than any global functions with the same names. Below is an example of a `LABELS` form that will return 12, the result of (+ 2 4 6), where 2, 4 and 6 are the results of evaluating the three local functions defined in the form.

    
    ;; Returns 12
    
    (labels ((first-function (x) (+ x x))
    	 (second-function (y) (* y y))
    	 (third-function (z) (first-function z)))
      (+ (first-function 1)
         (second-function 2)
         (third-function 3))) 


<a id="orgaf6eef4"></a>

# More on Functions


<a id="org4075850"></a>

## Lambda Expressions

Lambda expressions allow us to create unnamed functions. These are useful when writing small functions for certain tasks. Below is an example.

    
    ;; Lambda Form, returns 101
    
    ((lambda (x)
       (+ x 100))
     1)


<a id="org24845ae"></a>

## Function Parameters

By default, a function call must supply values for all parameters that feature in the function definition. We can modify this behaviour with the `&optional`, `&key` and `&rest` tokens.

The `&optional` token allows to distinguish between required parameters, placed before the `&optional` token, and optional parameters, placed after the token:

    
    (defun make-a-list (a b c d &optional e f g)
      (list a b c d e f g))
    
    
    ;; Returns (1 2 3 4 5 NIL NIL)
    
    (make-a-list 1 2 3 4 5)

One drawback of the `&optional` token, using the above as an example, is that we need to supply values for E and F if we want to supply the value for G, as arguments in a function call are assigned to the parameters in order.

To overcome this, we utilise the `&key` token to be able to specify which optional parameter we want to assign a value to. Below is an example of this.

    
    (defun make-a-list-2 (a b c d &key (e 1) f g)
      (list a b c d e f g))
    
    
    ;; Returns (1 2 3 4 1 NIL 7)
    
    (make-a-list-2 1 2 3 4 :g 7)

The preceding example also shows how we can supply a default value to an optional (setting E to 1 if no value for E is provided). When we called this function in the above, we set G to 7 and E also defaulted to 1. As no value was supplied for F, it defaulted to NIL.

In general, `&key` is preferable to &optional as it allows us to have greater control in our function calls. It also makes code easier to maintain and evolve as we can add new parameters to a function without affecting existing function calls (useful when writing libraries that are already being used by other programs).

Finally, the `&rest` token, placed before the last variable in a parameter list, allows us to write functions that can accept an unknown number of arguments. The last variable will be set to a list of all the remaining arguments supplied by the function call:

    
    (defun make-a-list-3 (a b c d &rest e) (list a b c d e))
    
    (make-a-list-3 1 2 3 4 5 6 7 8) ; (1 2 3 4 (5 6 7 8))

We can utilise multiple tokens in the same function call, as long as we declare them in order:

1.  First the names of required parameters are declared;
2.  Then the optional parameters;
3.  Then the rest parameter; and,
4.  Finally the keyword parameters are declared.


<a id="org8bfb896"></a>

## Multiple Values

The `VALUES` function returns multiple values and can be used as the last expression in the body of a function. The below example returns 1, NIL and 6 (individually, not as a list):

    
    (values 1 nil (+ 2 4))

If a `VALUES` function is supplied as an argument to a form which is only expecting one value, the first value returned by the `VALUES` function is used and the rest are discarded:

    
    ;; Returns 6
    
    (+ 5 (values 1 nil (+ 2 4)))

The `MULTIPLE-VALUE-BIND` macro is used to receive multiple values. The first argument of this macro is the variables and the second is the expression that returns their values. We can then use these values in the body of the multiple-value-bind macro. Below is an example.

    
    ;; Returns (1 2 3)
    
    (multiple-value-bind (x y z) (values 1 2 3)
      (list x y z)) 

If there are more variables than values, the leftover variables will be bound to NIL. If there are more values than variables, the extra values will be discarded. 


<a id="org54f0191"></a>

## Apply & Funcall

Functions in Lisp are first-class objects that generally support all operations available to other data objects, such as being modified, passed as an argument, returned from a function and being assigned to a variable.

The FUNCTION special operator (shorthand #') returns the function object associated with the name of function that is supplied as an argument:

    
    ;; Returns the function object
    
    (function +)
    
    ;; Equivalent syntax
    
    #'+

`APPLY` takes a function and a list of arguments for it and returns the result of applying the function to its arguments. Note how we have to use to sharp-quote (#') to pass the + function as an object into the APPLY function. Without doing so, Lisp will return an error as it will try to evaluate +, which is not legally permissible in the below example.

    
    ;; Returns 6
    
    (apply #'+ '(1 2 3))
    
    ;; Try also the following to see an example
    ;; of lambda expressions in use:
    
    (apply #'(lambda (a b)
    	   (+ a b))
           '(1 2 3))

The function `FUNCALL` is similar to `APPLY`, but allows us to pass arguments individually and not packaged as a list:

    
    ;; Returns 6
    
    (funcall #'+ 1 2 3)


<a id="org23997f8"></a>

## Mapping Functions

Mapping is a type of iteration in which a function is successively applied to pieces of one or more sequences. `MAPCAR` operates on successive elements of lists and returns a list of the result of the successive calls to the function specified. `MAPLIST` operates on successive CDRs of the lists.

    
    ;; Returns (-1 -2 -3)
    
    (mapcar #'(lambda (x) (- 0 x)) '(1 2 3))
    
    ;; Returns ((a b c d) (b c d) (c d) (d))
    
    (maplist #'(lambda (x) x) '(a b c d))

The above only work for lists. To map over other types of sequences, one can use `MAP`:

    
    (map result-type function &rest sequences)

Below are a couple of examples.

    
    ;; Returns a list ((#\a #\a) (#\b #\b) (#\c #\c))
    
    (map 'list #'(lambda (x) (list x x)) "abc")
    
    ;; Returns "1010"
    
    (map 'string
         #'(lambda (x) (if (oddp x) #\1 #\0))
         '(1 2 3 4))


<a id="orgd4ca0b8"></a>

# More on Lists


<a id="org5c772ed"></a>

## List Functions

The below are some common functions to access elements of a list:

    
    ;; Returns the element at the position given in
    ;; provided position (3), returning D:
    
    (nth 3 '(a b c d e f g))
    
    ;; Also try FIRST - TENTH for a similar effect:
    
    ;; Returns A
    
    (first '(a b c d e f g))
    
    ;; A similar version of NTH exists for accessing CDRs:
    ;; The below returns (D E F G)
    
    (nthcdr 3 '(a b c d e f g))
    
    ;; We can also easily access the last CDR with last
    ;; Below returns (G) (note it returns a CDR):
    
    (last '(a b c d e f g)) 

There are also some useful set functions that one can use in conjunction with sets. `MEMBER` and its variants might be the most useful:

    
    ;; Returns (B C)
    
    (member 'b '(a b c))
    
    ;; Returns (3 4)
    
    (member-if #'oddp '(2 3 4))
    
    ;; One can also specify the test to apply (default is EQL):
    
    (member 'b '(a b c) :test #'equal) 

`ADJOIN` joins an object onto a list only if it is not already a member:

    
    ;; Returns (A B C)
    
    (adjoin 'b '(a b c))
    
    ;; Returns (Z A B C)
    
    (adjoin 'z '(a b c))

Set union, intersection and complement operations can also be done:

    
    ;; Returns (A B C S)
    
    (union '(a b c)
           '(c b s))
    
    
    ;; Returns (C B)
    
    (intersection '(a b c)
    	      '(c b s))
    
    ;; Returns (A)
    
    (set-difference '(a b c) '(c b s))

The function `REDUCE` is useful to extend functions that only take two variables. It takes two arguments, a function (which must take exactly two values) and a sequence. The function is initially called on the first two elements of the sequence, and thereafter with each successive element as the second argument. The value returned by the last call is the value returned by the `REDUCE` function. For example, the below returns (A), the intersection of these three lists:

    
    (reduce #'intersection '((b r a d) (b a d) (c a t)))


<a id="org4bf54b9"></a>

## Push, Pop & Reverse

We can use lists as pushdown stacks. The macro PUSH can be used to push an element to the front of the list, while the macro POP can remove and return the first element of the list. Both are destructive operations as they directly change the original lists in question. For example:

    
    (defparameter my-list '(2 3 4))
    
    ;; Returns (1 2 3 4)
    
    (push 1 my-list)
    
    ;; Returns (1 2 3 4)
    
    my-list
    
    ;; Returns 1, the car of the list my-list
    
    (pop my-list)
    
    ;; Returns (2 3 4)
    
    my-list

`REVERSE` is a very useful function to reverse the order of elements within a list and is frequently used in various scenarios:

    
    ;; Returns (F E D C B A)
    
    (reverse '(a b c d e f))


<a id="org76442d1"></a>

## Association Lists

Association lists are a very useful data structure for mapping values to keys. They are lists of pairs (i.e. conses), with the key being the CAR of the pair and the datum being the CDR of the pair.

    
    ;; Example of an a-list:
    
    (defvar my-a-list '((one . 1) (two . 2)))
    
    ;; Add an entry to the front of an a-list
    ;; Returns ((one . 1) (two . 2) (three . 3))
    
    (acons three 3 my-a-list)
    
    ;; Create an a-list from lists of keys & datums:
    
    (pairlis '(one two three) '(1 2 3))
    
    ;; Return the pair associated with a key:
    ;; Below returns (one . 1)
    
    (assoc 'one my-a-list)
    
    ;; Find the first pair associated with a datum:
    ;; Returns (two . 2)
    
    (rassoc 2 my-a-list :test #'=)


<a id="orgbe1388e"></a>

# More on Sequences

Sequences are a data type in Lisp and lists, strings and arrays are all of type sequence.


<a id="org4ad89be"></a>

## Arrays

The function `MAKE-ARRAY` creates arrays. For example, we can create a 2 x 3 array as follows:

    
    (defparameter my-array
      (make-array '(2 3) :initial-element nil))

We can use `AREF` and `SETF` to access elements and set their values. The below will return NIL as the value of this element has not been set yet:

    
    (aref my-array 0 0)

The below will set the value of this element to B, which will be the value returned by the second form below:

    
    (setf (aref my-array 0 0) 'b)
    
    (aref my-array 0 0)

We use `:INITIAL-ELEMENT` to set the value of every element of an array to the provided argument. In the below, every element has a value of 2.

    
    (setf my-array
           (make-array '(2 3)
    		   :initial-element '((1 2 3) (1 2 3))))

The functions `ARRAY-RANK` and `ARRAY-DIMENSION` retrieve the the number of dimensions and the number of elements in a given dimension respectively:

    
    ;; Returns 2
    
    (array-rank my-array))
    
    ;; Returns 2
    
    (array-dimension my-array 0)
    
    ;; Returns 3
    
    (array-dimension my-array 1)

`:INITIAL-CONTENTS` is used to set the array to an object provided.

    
    (defparameter my-vector
      (make-array 3 :initial-contents '("a" 'b 3)))

A one-dimensional array is also known as a vector and the above example created one. Vectors can also be created with `VECTOR`:

    
    (vector "a" 'b 3)

The most famous vectors in LISP are strings. Strings are specialised vectors whose elements are characters.


<a id="org87d5a34"></a>

## Sequence Functions

Sequences have many useful functions. We can use `LENGTH` to return the number of items in a sequence. The below returns 6:

    
    (length '(a b c d e f))

`REMOVE` and its variants are very handy filter functions. The below returns (C R T) as a new list:

    
    (remove 'a '(c a r a t))

The below returns "cdbra", preserving only the last duplicate of each item:

    
    (remove-duplicates "abracadabra")

The below returns (2 4 4) by removing all odd numbers:

    
    (remove-if #'oddp '(1 2 3 4 4))

`SUBSEQ` can extract a portion of a sequence. Its arguments are a list, the starting position and an optional ending position (which is not to be included in the subsequence). The below will return (B C D):

    
    (subseq '(a b c d e f) 1 4)

`SORT` takes a sequence and a comparison function of two arguments and destructively (i.e. by modifying the original sequence) returns a sequence sorted according to the function. The below returns (6 5 4 2 1) by sorting in descending order:

    
    (sort '(1 4 2 5 6) #'>)

The functions `EVERY` and `SOME` test whether a sequence satisfies a provided predicate. The below will return NIL as not every item is odd:

    
    (every #'oddp '( 1 2 5))

The below will return T as some of the items are odd:

    
    (some #'oddp '( 1 2 5))

The below will return T as the items of the first sequence are greater than those of the second sequence in an element-wise comparison:

    
    (every #'> '(1 3 5) '(0 2 4))

We can find elements within a sequence with `FIND`, which returns the leftmost such element, or `POSITION`, which returns the position of such an item, as an integer. The below returns 1, the item we are searching for:

    
    (find 1 '(1 2 3 4))

The below returns 0, the position of the item we are searching for:

    
    (position 1 '(1 2 3 4))

We can use `COUNT` to count the number of instances of the element within the sequence. The below returns 3, the number of instances of 1 in the list:

    
    (count 1 '(1 2 3 1 1 4))

We can use `SEARCH` to search for sequence within another. The below returns 4, the position of the string "Hello" in "Hi! Hello, World!":

    
    (search "Hello" "Hi! Hello, World!")


<a id="org6ce7715"></a>

## Keyword Arguments

Many list and sequence functions take one or more keyword arguments from the below table. For example, we can use `POSITION` to return the position of an element within a sequenc (or NIL if not found) and use keyword arguments to determine where to begin the search.

The below will return 4:

    
    (position #\a "fantasia" :start 3 :end 7)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Parameter</th>
<th scope="col" class="org-left">Position</th>
<th scope="col" class="org-left">Default</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">:key</td>
<td class="org-left">A function to apply to each element</td>
<td class="org-left">identity</td>
</tr>


<tr>
<td class="org-left">:test</td>
<td class="org-left">The test function for comparison</td>
<td class="org-left">eql</td>
</tr>


<tr>
<td class="org-left">:from-end</td>
<td class="org-left">If true, work backwards</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">:start</td>
<td class="org-left">Position at which to start</td>
<td class="org-left">0</td>
</tr>


<tr>
<td class="org-left">:end</td>
<td class="org-left">Position, if any, at which to stop</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="org15a50bd"></a>

# Data Structures


<a id="orgf297da4"></a>

## Hash Tables

A hash table is a way of associating pairs of objects, like a dictionary. The objects stored in a hash table or used as keys can be of any type. We can make hastables with `MAKE-HASH-TABLE` and retrieve values associated with a given key with `GETHASH`:

    
    (defparameter my-hash-table (make-hash-table))
    
    ;; Returns NIL as not yet set
    
    (gethash 'color my-hash-table) 

Similar to other structures, we use `SETF` to set values. Hash tables can accommodate any number of elements, because they are expanded when they run out of space.

    
    (setf (gethash 'color my-hash-table) 'red)  

The function `MAPHASH` allows you to iterate over all entries in the hash table.

-   Its first argument must be a function which accepts two arguments, the key and the value of each entry
-   Note that due to the nature of hash tables you can't control the order in which the entries are provided to `MAPHASH` (or other traversing constructs)

Below is an example, which will return COLOR = RED as there is only one item (COLOR) currently in our hash table.

    
    (maphash #'(lambda (key value)
    	     (format t "~A = ~A~%" key value))
    	 my-hash-table)

You can remove items from a hash table with `REMHASH`:

    
    (remhash 'color my-hash-table)


<a id="orgb12745c"></a>

## Structures

Common Lisp provides the `DEFSTRUCT` facility for creating data structures with named components. This makes it easier to manipulate custom data objects as we can refer to their components by name.

Constructor, access and assignment constructs are automatically defined when a data type is defined through `DEFSTRUCT`. Consider the below example of defining a data type for rectangles.

`DEFSTRUCT` defines RECTANGLE to be a structure with two fields, HEIGHT and WIDTH. The symbol RECTANGLE becomes the name of a data type and each rectangle will be of type RECTANGLE, then STRUCTURE, then ATOM and then T.

`DEFSTRUCT` will generate four associated functions:

1.  RECTANGLE-HEIGHT and RECTANGLE-WIDTH to access elements of the
    structure

2.  RECTANGLE-P to test whether an object is of type RECTANGLE

3.  MAKE-RECTANGLE to create rectangles

4.  COPY-RECTANGLE to create copies of rectangles

Below is an example of the above structure.

    
    ;; Height will default to NIL while width will default to 5 
    
    (defstruct rectangle
      (height)
      (width 5))

The below creates an instance of RECTANGLE:

    
    (defvar rectangle-1)
    
    (setf rectangle-1
          (make-rectangle :height 10 :width 15))

The below will return 10:

    
    (rectangle-height rectangle-1)

The below will set RECTANGLE-WIDTH of RECTANGLE-1 to 20:

    
    (setf (rectangle-width rectangle-1) 20)


<a id="org558a0c8"></a>

## Common Lisp Object System (CLOS)

Below is an example of creating two classes, one which inherits from the other. Courtesy of the [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/clos.html).

    
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

Accessor functions are used for both getting and setting. The below will return DAVID.

    
    (name person-1)

The below will set name to TOM.

    
    (setf (name person-1) "Tom")

`:INITFORM` is used to set default values. The below returns "Yes" (the default value) as the value of LISPER was not yet set.

    
    (lisper person-1)

The below is an example of inheriting from the PERSON class.

    
    (defclass child (person)
      ((can-walk-p
       :initarg :can-walk-p
       :initform "No"
       :accessor can-walk-p)))

Inherited classes inherit the slots of their parents. CHILD will inherit LISPER from PERSON. The below will return "Yes":

    
    (lisper (make-instance 'child :name "Phoebe"))

Inherited classes can also introduce new slots. CHILD introduces CAN-WLAK-P. The below will return "No":

    
    (can-walk-p (make-instance 'child))

We can add methods to classes with a combination of `DEFGENERIC` and `DEFMETHOD`. Note that Common Lisp supports multiple dispatch so that many classes can share and use the same method names.

`DEFGENERIC` establishes an entry in the method dispatch table, while `DEFMETHOD` allows us to create specialised versions.

    
    ;; Version with a default method (to be used if no other specialisations exist:
    
    (defgeneric greet (obj)
      (:documentation "Says hi")
      (:method (obj)
    	   (format t "Hi")))
    
    ;; Version without default method:
    
    (defgeneric greet-2 (obj1 obj2)
      (:documentation "Says hi"))

In creating specialised methods, we add the parameter type to the parameter list. In a method call, LISP will then use the method which matches the parameter types of the parameters supplied in the method call.

In the below, GUEST-NAME is a parameter of type PERSON, while MESSAGE is a parameter that is not specialised and can be anything.

    
    (defmethod greet-2 ((guest-name person) message)
      (format t "The person greets ~A and says ~A" guest-name message))

Finally, it is useful to create custom print output for CLOS objects. This can be achieved with the following.

    
      (defmethod print-object ((obj person) stream)
      (print-unreadable-object (obj stream :type t)
    			   (format stream "~a" (name obj))))
    
    ;; Returns #<Person Tom>
    
      (print person-1)


<a id="org4bfaf56"></a>

# Other


<a id="org983e82d"></a>

## Reading & Writing to Files

The `WITH-OPEN-FILE` macro is used to read and write to files. Below is an example of opening a file and then reading from it. The `NIL` in the below inhibits end of file errors.

    
    (with-open-file (my-stream "/Users/ashokkhanna/test.txt")
      (format t "~a~%" (read-line my-stream nil)))

Below is an example of opening a file and then writing to it.

    
    (with-open-file (my-stream "/Users/ashokkhanna/test.txt" :direction
    			   :output :if-exists :append)
      (format my-stream "~a~%" "Hello, World!"))

The following open arguments can be supplied to the `WITH-OPEN-FILE` macro:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Arguments</th>
<th scope="col" class="org-left">Effect</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">:direction :output</td>
<td class="org-left">Write to a file insead of reading</td>
</tr>


<tr>
<td class="org-left">:if-does-not-exist :create</td>
<td class="org-left">Create a file if it does not exist</td>
</tr>


<tr>
<td class="org-left">:if-exists :supersede</td>
<td class="org-left">Replace the file that exists</td>
</tr>


<tr>
<td class="org-left">:if-exists :overwrite</td>
<td class="org-left">Overwrite file</td>
</tr>


<tr>
<td class="org-left">:if-exists :append</td>
<td class="org-left">Write to end of file</td>
</tr>
</tbody>
</table>


<a id="org4060968"></a>

## Packages

Packages are a central mechanism for avoiding name collisions that occur if multiple files contain variables or functions with the same name. More information on packages can be found on [my guide on Medium](https://ashok-khanna.medium.com/an-introduction-to-lisp-packages-7a9ee352006e).

Packages need to be registered before they can be used. Below is an example of registering a package that inherits from two packages and exports two symbols. This example also shadows two symbols,  allowing us to use, within MY-PACKAGE, the definitions of these symbols (RESTART and CONDITION in our case) that exist within the package MY-PACKAGE and not definitions of these symbols inherited from other packages (CL in our case, where RESTART and CONDITION are interned also).

    
    (defpackage :my-package
      (:use :cl :other-package-1)
      (:export :symbol-1
    	   :restart)
      (:shadow :restart
    	   :condition))

Once a package is registered with the above, we can switch to it with `IN-PACKAGE`.

-   **Within a package**, all symbols defined in that package are accessible. In addition, any exported symbols from packages inherited via the `:USE` command can be directly accessed without a package qualifier

-   **Outside of a package**, internal symbols can be accessed via a double-colon package qualifier, e.g. `my-package::symbol-3`, while exported symbols can be accessed via a single-colon package qualifier, e.g. `my-package:symbol-1`

It is good practice to put the above at the top of LISP files so that readers can easily follow which package is currently live in a file.

    
    (in-package :my-package)

Finally, note in the above, that we will get an error if we try to inherit from both CL and MY-PACKAGE due to to the clash in symbols RESTART and CONDITION that appear in both packages. To overcome this, we can use a `:shading-import-from:` command, such as in the following.

    
    (defpackage :my-package-2
      (:use :cl :my-package)
      (:shadowing-import-from :my-package :restart))

