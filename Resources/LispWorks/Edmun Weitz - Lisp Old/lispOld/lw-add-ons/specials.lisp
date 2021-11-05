;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/specials.lisp,v 1.41 2015/03/06 12:54:25 edi Exp $

;;; Copyright (c) 2005-2015, Dr. Edmund Weitz.  All rights reserved.

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

;;; This code copied almost verbatim from SLIME, see
;;; <http://common-lisp.net/project/slime/>

(in-package :lw-add-ons)

(defvar *show-doc-string-when-showing-arglist* nil
  "Whether the editor command \"Insert Space and Show Arglist\"
is supposed to show the documentation string as well.")

(defvar *max-completions-to-show* 14
  "The maximum number of possible completions shown in the echo
area by \"Complete Symbol Without Dialog.\"")

(defvar *insert-right-parenthesis-if-no-args* t
  "Whether \"Complete Symbol Without Dialog\" should insert a
right parenthesis if the function is known to have an empty
argument list.")

(defvar *mop-page* "c:/home/lisp/doc/mop/dictionary.html"
  "A pathname specifier denoting the location of the dictionary
page from the AMOP HTML version.  The page is available online at
<http://www.lisp.org/mop/dictionary.html>")

(defvar *completion-match-function* 'compound-prefix-match
  "The function used by \"Complete Symbol Without Dialog\" to
check possible completions.  Should be a designator for a
function of two arguments and return true iff the second argument
is a possible completion of the first one.")

(defvar *use-abbreviated-complete-symbol* t
  "Whether \"Indent And Complete Symbol\" should call \"Abbreviated
Complete Symbol\" \(only available in LispWorks 5.1 or higher) instead
of \"Complete Symbol Without Dialog\".")

(defvar *make-backup-filename-function* nil
  "If the value of this variable is not NIL, then it should be a
designator for a function of one argument which accepts a pathname and
returns a pathname.  LispWork's own EDITOR::MAKE-BACKUP-FILENAME
function will be replaced with this one in this case.")

(defvar *backup-directory*
  #+(or :win32 :macosx)
  (merge-pathnames "LW-ADD-ONS/Backups/"
                   (probe-file
                    (sys:get-folder-path #+:win32 :local-appdata
                                         #+:macosx :my-appsupport
                                         :create t)))
  #+:linux #p"~/.lw-backups/"
  "The directory where backups are stored if the value of
*MAKE-BACKUP-FILENAME-FUNCTION* denotes the function
'MAKE-BACKUP-FILENAME-USING-BACKUP-DIRECTORY.  It is recommended that
you dont't use this directory for other purposes.")

(defvar *swank-loader-pathname* #p"c:/emacs/site-lisp/slime/swank-loader.lisp"
  "A pathname specifier denoting the location of the
`swank-loader.lisp' file.  Only needed if one wants to start the
Swank server from LW - see function START-SWANK-SERVER.")

(defvar *translate-asdf-systems* t
  "Whether ASDF systems should be automatically converted to LispWorks
Common Defsystem systems.")

(defvar *max-info-length* 400
  "The maximum length \(in characters) of a message shown by
SHOW-INFO \(unless FULL-LENGTH-P is true).")

(defvar *apropos-max-search-list-length* 20
  "The maximal number of items in the CAPI:TEXT-INPUT-CHOICE in the
Apropos Dialog.")

(defvar *apropos-max-string-length* 50
  "The maximum amount of characters to show when an object is printed
in the pull down menu of an Apropos Dialog.")

(defvar *apropos-print-length* 5
  "*PRINT-LENGTH* is bound to this value while the Apropos Dialog
displays objects.")

(defvar *apropos-print-level* 5
  "*PRINT-LEVEL* is bound to this value while the Apropos Dialog
displays objects.")

(defvar *product-registry-path* '("Software" "Edi Weitz" "LW-ADD-ONS")
  "The product registry path used for storing and retrieving user
preferences.")

(defconstant +apropos-headline+ '("Symbol Name" "Package" "Fun" "Var" "Class" "Exp")
  "The headline of the Apropos Dialog's result panel.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *use-quicklisp-for-shortcut-l* t
    "Whether listener shortcuts should prefer Quicklisp."))

(defvar *listener-shortcuts*
  (load-time-value
   (append
    (list '("c" . "Compile ASDF System")
          '("t" . "Test ASDF System")
          '("p" . "Change Package")
          '("i" . "Change Package")
          '("cd" . "Change Directory")
          '("pwd" . "Show Directory")
          '("q" . "Quit")
          '("s" . "Quit"))
    #-:quicklisp
    (list '("l" . "Load ASDF System"))
    #+:quicklisp
    (if *use-quicklisp-for-shortcut-l*
      (list '("a" . "Load ASDF System") '("l" . "Quickload Library"))
      (list '("l" . "Load ASDF System") '("ql" . "Quickload Library")))))
  "An alist of commands that can be invoked with \"Invoke Listener
Shortcut\" or with comma at beginning of listener line, each one
preceded by a shortcut.")

(defvar *swank-started-p* nil
  "Whether START-SWANK-SERVER has already been called.")

(defvar *doc-hash* (make-hash-table :test #'equalp)
  "A hash table which maps entries \(mostly strings) for the
\"Meta Documentation\" command to URLs.")
(defvar *doc-hash-entries* nil
  "The list of all keys of *DOC-HASH*.")

(defvar *hyperdoc-packages* nil
  "Temporarily set to a list of all packages that have a symbol named
HYPERDOC-LOOKUP during execution of \"Meta Documentation\" command.")
(defvar *doc-entries* nil
  "Temporarily set to a list of all candidates during completion in
\"Meta Documentation\" command.")

(defconstant +cl-user-package+ (load-time-value (find-package :cl-user))
  "The CL-USER package.")
(defconstant +keyword-package+ (load-time-value (find-package :keyword))
  "The KEYWORD package.")

(defvar *all-asdf-systems* nil
  "Temporarily bound to a list of all ASDF system names while
prompting for a system name.")

(defvar *search-end* nil
  "If this variable is bound to a true value then it should be a
pointer and EDITOR::FIND-PATTERN \(and EDITOR:I-FIND-PATTERN) won't
search beyond this point \(unless called with a non-NIL LIMIT
argument).")

(defvar *change-default-for-file-prompt* nil
  "If this variable is bound to a a true value then the function
EDITOR:PROMPT-FOR-FILE will use the full file name \(as opposed
to the file's location) as its default string \(unless a default
string was explicitly specified or the DEFAULT argument is a
string).")

#+:editor-does-not-have-go-back
(defvar *find-definitions-stack* nil
  "Stack of previous positions \(points) within the editor, used by
new \"Pop Definitions Stack\" command.  See docs.")

(defvar *lw-add-ons-break-on-signals* nil
  "The value *BREAK-ON-SIGNALS* is bound to in IGNORE-ERRORS*.  Set
this to NIL to debug LW-ADD-ONS.")

(defvar *temp-files* nil
  "A list of temporary files which should be deleted when the image
exits.")

(defvar *clhs-add-ons*
  '(("~C: Character" "22_caa.htm")
    ("~%: Newline" "22_cab.htm")
    ("~&: Freshline" "22_cac.htm")
    ("~|: Page" "22_cad.htm")
    ("~~: Tilde" "22_cae.htm")
    ("~R: Radix" "22_cba.htm")
    ("~D: Decimal" "22_cbb.htm")
    ("~B: Binary" "22_cbc.htm")
    ("~O: Octal" "22_cbd.htm")
    ("~X: Hexadecimal" "22_cbe.htm")
    ("~F: Fixed-Format Floating-Point" "22_cca.htm")
    ("~E: Exponential Floating-Point" "22_ccb.htm")
    ("~G: General Floating-Point" "22_ccc.htm")
    ("~$: Monetary Floating-Point" "22_ccd.htm")
    ("~A: Aesthetic" "22_cda.htm")
    ("~S: Standard" "22_cdb.htm")
    ("~W: Write" "22_cdc.htm")
    ("~_: Conditional Newline" "22_cea.htm")
    ("~<: Logical Block" "22_ceb.htm")
    ("~I: Indent" "22_cec.htm")
    ("~/: Call Function" "22_ced.htm")
    ("~T: Tabulate" "22_cfa.htm")
    ("~<: Justification" "22_cfb.htm")
    ("~>: End of Justification" "22_cfc.htm")
    ("~*: Go-To" "22_cga.htm")
    ("~[: Conditional Expression" "22_cgb.htm")
    ("~]: End of Conditional Expression" "22_cgc.htm")
    ("~{: Iteration" "22_cgd.htm")
    ("~}: End of Iteration" "22_cge.htm")
    ("~?: Recursive Processing" "22_cgf.htm")
    ("~(: Case Conversion" "22_cha.htm")
    ("~): End of Case Conversion" "22_chb.htm")
    ("~P: Plural" "22_chc.htm")
    ("~;: Clause Separator" "22_cia.htm")
    ("~^: Escape Upward" "22_cib.htm")
    ("~NEWLINE: Ignored Newline" "22_cic.htm")
    ("\(" "02_da.htm")
    (")" "02_db.htm")
    ("'" "02_dc.htm")
    (";" "02_dd.htm")
    ("\"" "02_de.htm")
    ("`" "02_df.htm")
    ("," "02_dg.htm")
    ("#" "02_dh.htm")
    ("#\\" "02_dha.htm")
    ("#'" "02_dhb.htm")
    ("#\(" "02_dhc.htm")
    ("#*" "02_dhd.htm")
    ("#:" "02_dhe.htm")
    ("#." "02_dhf.htm")
    ("#b" "02_dhg.htm")
    ("#o" "02_dhh.htm")
    ("#x" "02_dhi.htm")
    ("#r" "02_dhj.htm")
    ("#c" "02_dhk.htm")
    ("#a" "02_dhl.htm")
    ("#s" "02_dhm.htm")
    ("#p" "02_dhn.htm")
    ("#=" "02_dho.htm")
    ("##" "02_dhp.htm")
    ("#+" "02_dhq.htm")
    ("#-" "02_dhr.htm")
    ("#|" "02_dhs.htm")
    ("#<" "02_dht.htm")
    ("loop:with" "06_abb.htm")
    ("loop:for-as-..." "06_aba.htm")
    ("loop:for-as-arithmetic" "06_abaa.htm")
    ("loop:for-as-in-list" "06_abab.htm")
    ("loop:for-as-on-list" "06_abac.htm")
    ("loop:for-as-equals-then" "06_abad.htm")
    ("loop:for-as-across" "06_abae.htm")
    ("loop:for-as-hash" "06_abaf.htm")
    ("loop:for-as-package" "06_abag.htm")            
    ("loop:collect" "06_ac.htm")
    ("loop:append" "06_ac.htm")
    ("loop:nconc" "06_ac.htm")
    ("loop:count" "06_ac.htm")
    ("loop:maximize" "06_ac.htm")
    ("loop:minimize" "06_ac.htm")
    ("loop:sum" "06_ac.htm")
    ("loop:repeat" "06_ad.htm")
    ("loop:always" "06_ad.htm")
    ("loop:never" "06_ad.htm")
    ("loop:thereis" "06_ad.htm")
    ("loop:while" "06_ad.htm")
    ("loop:until" "06_ad.htm")
    ("loop:do" "06_ae.htm")
    ("loop:return" "06_ae.htm")
    ("loop:if" "06_af.htm")
    ("loop:when" "06_af.htm")
    ("loop:unless" "06_af.htm")
    ("loop:else" "06_af.htm")
    ("loop:it" "06_af.htm")
    ("loop:end" "06_af.htm")
    ("loop:named" "06_aga.htm")
    ("loop:initially" "06_agb.htm")
    ("loop:finally" "06_agb.htm")
    (":test" "17_ba.htm")
    (":test-not" "17_ba.htm")
    (":key" "17_bb.htm")
    (":eof-error-p" "23_aca.htm")
    (":recursive-p" "23_acb.htm"))
  "A couple of additions to the `standard' CLHS entries that can
be found in the symbol-index.")


(defparameter *mop-links*
  '(("add-dependent" "#add-dependent")
    ("add-direct-method" "#add-direct-method")
    ("add-direct-subclass" "#add-direct-subclass")
    ("add-method" "#add-method")
    ("allocate-instance" "#allocate-instance")
    ("class-... " "#class-")
    ("class-default-initargs" "#class-mo-readers")
    ("class-direct-default-initargs" "#class-mo-readers")
    ("class-direct-slots" "#class-mo-readers")
    ("class-direct-subclasses" "#class-mo-readers")
    ("class-direct-superclasses" "#class-mo-readers")
    ("class-finalized-p" "#class-mo-readers")
    ("class-name" "#class-mo-readers")
    ("class-precedence-list" "#class-mo-readers")
    ("class-prototype" "#class-mo-readers")
    ("class-slots" "#class-mo-readers")
    ("compute-applicable-methods" "#compute-applicable-methods")
    ("compute-applicable-methods-using-classes" "#compute-applicable-methods-using-classes")
    ("compute-class-precedence-list" "#compute-class-precedence-list")
    ("compute-default-initargs" "#compute-default-initargs")
    ("compute-discriminating-function" "#compute-discriminating-function")
    ("compute-effective-method" "#compute-effective-method")
    ("compute-effective-slot-definition" "#compute-effective-slot-definition")
    ("compute-slots" "#compute-slots")
    ("direct-slot-definition-class" "#direct-slot-definition-class")
    ("effective-slot-definition-class" "#effective-slot-definition-class")
    ("ensure-class" "#ensure-class")
    ("ensure-class-using-class" "#ensure-class-using-class")
    ("ensure-generic-function" "#ensure-generic-function")
    ("ensure-generic-function-using-class" "#ensure-generic-function-using-class")
    ("eql-specializer-object" "#eql-specializer-object")
    ("extract-lambda-list" "#extract-lambda-list")
    ("extract-specializer-names" "#extract-specializer-names")
    ("finalize-inheritance" "#finalize-inheritance")
    ("find-method-combination" "#find-method-combination")
    ("funcallable-standard-instance-access" "#funcallable-standard-instance-access")
    ("generic-function-..." "#generic-function-")
    ("generic-function-argument-precedence-order" "#gf-mo-readers")
    ("generic-function-declarations" "#gf-mo-readers")
    ("generic-function-lambda-list" "#gf-mo-readers")
    ("generic-function-method-class" "#gf-mo-readers")
    ("generic-function-method-combination" "#gf-mo-readers")
    ("generic-function-methods" "#gf-mo-readers")
    ("generic-function-name" "#gf-mo-readers")
    ("Initialization of Class Metaobjects" "#class-mo-init")
    ("Initialization of Generic Function Metaobjects" "#gf-mo-init")
    ("Initialization of Method Metaobjects" "#Initialization")
    ("Initialization of Slot Definition Metaobjects" "#Initialization")    
    ("intern-eql-specializer" "#intern-eql-specializer")
    ("make-instance" "#make-instance")
    ("make-method-lambda" "#make-method-lambda")
    ("map-dependents" "#map-dependents")
    ("method-..." "#method-")
    ("method-function" "#method-mo-readers")
    ("method-generic-function" "#method-mo-readers")
    ("method-lambda-list" "#method-mo-readers")
    ("method-specializers" "#method-mo-readers")
    ("method-qualifiers" "#method-mo-readers")
    ("accessor-method-slot-definition" "#method-mo-readers")
    ("Readers for Class Metaobjects" "#class-mo-readers")
    ("Readers for Generic Function Metaobjects" "#gf-mo-readers")
    ("Readers for Method Metaobjects" "#method-mo-readers")
    ("Readers for Slot Definition Metaobjects" "#slotd-mo-readers")
    ("reader-method-class" "#reader-method-class")
    ("remove-dependent" "#remove-dependent")
    ("remove-direct-method" "#remove-direct-method")
    ("remove-direct-subclass" "#remove-direct-subclass")
    ("remove-method" "#remove-method")
    ("set-funcallable-instance-function" "#set-funcallable-instance-function")
    ("\(setf class-name)" "#\(setf class-name)")
    ("\(setf generic-function-name)" "#\(setf generic-function-name)")
    ("\(setf slot-value-using-class)" "#\(setf slot-value-using-class)")
    ("slot-boundp-using-class" "#slot-boundp-using-class")
    ("slot-definition-..." "#slot-definition-")
    ("slot-definition-allocation" "#slotd-mo-readers")
    ("slot-definition-initargs" "#slotd-mo-readers")
    ("slot-definition-initform" "#slotd-mo-readers")
    ("slot-definition-initfunction" "#slotd-mo-readers")
    ("slot-definition-location" "#slotd-mo-readers")
    ("slot-definition-name" "#slotd-mo-readers")
    ("slot-definition-readers" "#slotd-mo-readers")
    ("slot-definition-writers" "#slotd-mo-readers")
    ("slot-definition-type" "#slotd-mo-readers")
    ("slot-makunbound-using-class" "#slot-makunbound-using-class")
    ("slot-value-using-class" "#slot-value-using-class")
    ("specializer-direct-generic-functions" "#specializer-direct-generic-functions")
    ("specializer-direct-methods" "#specializer-direct-methods")
    ("standard-instance-access" "#standard-instance-access")
    ("update-dependent" "#update-dependent")
    ("validate-superclass" "#validate-superclass")
    ("writer-method-class" "#writer-method-class"))
  "URL fragments for all relevant entries in the MOP dictionary
page.")
