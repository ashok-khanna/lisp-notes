
;; * - * - * - * - * - * - * - * - * - * - * - *

;; Working Example for Persistent In-Memory 
;; Data Storage in Common Lisp
;; Tested on SBCL / MacOS
;; Date: 22 June 2021

;; * - * - * - * - * - * - * - * - * - * - * - *



;; * - * - * - * - * - * - * - * - * - * - * - *

;; 1.0 Installation & Setup

;; * - * - * - * - * - * - * - * - * - * - * - *

;; Load from QuickLisp:

(ql:quickload :bknr.datastore)

;; Create the Data Store:

(make-instance 'bknr.datastore:mp-store
	       :directory "/Users/ashokkhanna/bknr/tmp/object-store/"
                         :subsystems (list
                                      (make-instance
				       'bknr.datastore:store-object-subsystem)))
;; Close the store:

(bknr.datastore:close-store)

;; Load it again:

(make-instance 'bknr.datastore:mp-store
	       :directory "/Users/ashokkhanna/bknr/tmp/object-store/"
                         :subsystems (list
                                      (make-instance
				       'bknr.datastore:store-object-subsystem)))



;; * - * - * - * - * - * - * - * - * - * - * - *

;; 2.0 Creating Storeable CLOS Objects

;; * - * - * - * - * - * - * - * - * - * - * - *

;; Define our CLOS objects (note that this should
;; actually go before 1.0 above, make this change
;; when you have a moment)

(defclass book (bknr.datastore:store-object)
  ((author :accessor book-author
    :initarg :author)
   (title :accessor book-title
   :initarg :title)
   (book-id :accessor book-id
       :initarg :book-id)
   (subject :accessor book-subject
     :initarg :subject))
  (:metaclass bknr.datastore:persistent-class))

;; Create instances of our CLOS object:

(make-instance 'book :author "Guy Steele" :title "Common Lisp the Language"
	       :book-id 1 :subject (list "Lisp" "Common Lisp"))

(make-instance 'book
	       :author "Gerald Jay Sussman and Hal Abelson"
	       :title "Structure and Interpretation of Computer Programming"
	       :book-id 2
	       :subject (list "Scheme"))

(make-instance 'book :author "Paul Graham" :title "ANSI Common Lisp"
	       :book-id 3 :subject (list "Lisp" "Common Lisp"))

(make-instance 'book :author "Paul Graham" :title "On Lisp"
	       :book-id 4 :subject (list "Lisp" "Macros"))




;; * - * - * - * - * - * - * - * - * - * - * - *

;; 3.0 Accessing Objects

;; * - * - * - * - * - * - * - * - * - * - * - *


;; Retrieve object by datastore ID:

(bknr.datastore:store-object-with-id 2)

;; Retrieve all objects in the datastore:

(bknr.datastore:all-store-objects)

;; Retrieve all objects of a given class:

(bknr.datastore:store-objects-with-class 'book)




;; * - * - * - * - * - * - * - * - * - * - * - *

;; 4.0 Modifying and Deleting Objects

;; * - * - * - * - * - * - * - * - * - * - * - *

;; Modifying Objects:

(bknr.datastore:with-transaction ()
  (setf (book-author
         (bknr.datastore:store-object-with-id 3))
        "Paul"))

;; Deleting Objects:

(bknr.datastore:delete-object
 (bknr.datastore:store-object-with-id 2))




;; * - * - * - * - * - * - * - * - * - * - * - *

;; 5.0 Taking Snapshots & Restoring from them

;; * - * - * - * - * - * - * - * - * - * - * - *


;; Take a snapshot:

(bknr.datastore:snapshot)

;; Retrieve from a snapshot:

;; --> Just do the old copy/paste trick by
;; copying the files from the previous
;; snapshot into the "current" folder




;; * - * - * - * - * - * - * - * - * - * - * - *

;; 6.0 Importing / Exporting to XML

;; * - * - * - * - * - * - * - * - * - * - * - *

;; Make sure to download/use the books.dtd file
;; saved in the Repo (see second step below)


;; Load the BKNR.IMPEX package:

(ql:quickload :bknr.impex)


;; Load our XML DTD:

(defvar *book-dtd* "/Users/ashokkhanna/bknr/book.dtd")


;; Create custom XML class:

(defclass book-xml ()
  ((author :accessor book-author
           :initarg :author :element "author")
   (title :accessor book-title
          :initarg :title :element "title")
   (id :accessor book-id
       :initarg :id :attribute "id"
       :parser #'parse-integer
       :index-type bknr.indices:unique-index
       :index-values all-books-xml)
   (subject :accessor book-subject
            :initarg :subject :attribute "subject"))
  (:metaclass bknr.impex:xml-class)
  (:dtd-name *book-dtd*)
  (:element "book"))


;; Create some instances of this CLOS object:

(make-instance 'book-xml
               :author "Guy Steele"
               :title "Common Lisp the Language, 2nd Edition"
               :id 1 :subject "Common Lisp")

(make-instance 'book-xml
                :author "Gerald Jay Sussman and Hal Abelson"
                :title "Structure and Interpretation of Computer Programming"
                :id 2 :subject "Scheme")


;; Export to XML:

(bknr.impex:write-to-xml (all-books-xml) :name "books")

(with-open-file (*standard-output* "/Users/ashokkhanna/bknr/book-export.xml"
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
  (bknr.impex:write-to-xml (all-books-xml) :name "books"))


;; Import from XML:

;; Error if you run in current order (refer guide):

;; (bknr.impex:parse-xml-file "/Users/ashokkhanna/bknr/book-export.xml"
;;                            (list (find-class 'book-xml)))

;; As we first need to clear the indices as these IDs already exist:

(bknr.impex::clear-class-indices (find-class 'book-xml))

;; Now try importing from XML:

(bknr.impex:parse-xml-file "/Users/ashokkhanna/bknr/book-export.xml"
                           (list (find-class 'book-xml)))


;; Let's clear the indicies again (this can get annoying!):

(bknr.impex::clear-class-indices (find-class 'book-xml))

;; Exporting from the datastore to XML:

(let ((book-data (bknr.datastore:store-objects-with-class 'book)))
    (loop for item in book-data
       do (make-instance 'book-xml
                         :author (book-author item)
                         :title (book-title item)
                         :id (book-id item)
                         :subject (book-subject item))))

;; Check to see it works:

(all-books-xml)




;; * - * - * - * - * - * - * - * - * - * - * - *

;; 7.0 Adding Custom Indices

;; * - * - * - * - * - * - * - * - * - * - * - *


;; Re-define our class with custom indices:

;; Class Definition - Title, ID must be unique
;; Author can repeat and subject is a list of subjects

(defclass book-custom-indices (bknr.datastore:store-object)
  ((author :accessor book-author
	   :initarg :author
	   :index-type bknr.indices:hash-index
	   :index-initargs (:test #'equal)
	   :index-reader books-with-author
	   :index-values all-authors)
   (title :accessor book-title
	  :initarg :title
	  :index-type bknr.indices:unique-index
	  :index-initargs (:test #'equal)
	  :index-reader book-with-title
	  :index-values all-titles)
   (book-id :accessor book-bci-id
       :initarg :book-id
       :index-type bknr.indices:unique-index
       :index-initargs (:test #'equal)
       :index-reader book-with-book-id
       :index-values all-book-ids
       :index-mapvalues with-book-ids)
   (subject :accessor book-subject
	    :initarg :subject
	    :index-type bknr.indices:hash-list-index
	    :index-initargs (:test #'equal)
	    :index-reader books-with-subject
	    :index-values all-subjects))
  (:metaclass bknr.datastore:persistent-class))


;; Example Objects:

(make-instance 'book-custom-indices
	       :author "Guy Steele"
	       :title "Common Lisp the Language"
	       :book-id 1
	       :subject (list "Lisp" "Programming" "Common Lisp"))

(make-instance 'book-custom-indices
	       :author "Gerald Jay Sussman and Hal Abelson"
	       :title "Structure and Interpretation of Computer Programming"
	       :book-id 2
	       :subject (list "Scheme" "Programming"))

(make-instance 'book-custom-indices
	       :author "Paul Graham"
	       :title "ANSI Common Lisp"
	       :book-id 3
	       :subject (list "Lisp" "Programming" "Common Lisp"))

(make-instance 'book-custom-indices
	       :author "Paul Graham"
	       :title "On Lisp"
	       :book-id 4
	       :subject (list "Lisp" "Macros" "Common Lisp"))

;; Error, ID needs to be unique:

;; (make-instance 'book-custom-indices
;; 	       :author "Paul Graham"
;; 	       :title "Hackers & Painters"
;; 	       :book-id 4
;; 	       :subject (list "Lisp" "Art"))


;; Now let's try out our accessor functions:

;; Returns two books:
(books-with-author "Paul Graham")

;; Returns one book:
(book-with-title "ANSI Common Lisp")

;; Returns one book:
(book-with-book-id 1)

;; Returns three books:
(books-with-subject "Lisp")




;; * - * - * - * - * - * - * - * - * - * - * - *

;; 8.0 Adding Custom Indices (Part 2)

;; * - * - * - * - * - * - * - * - * - * - * - *

;; More granular accessor functions:

(defun get-author-title (author title)
  (let ((all-author-books (all-authors)))
    (loop for book in all-author-books
       if (equal (book-title book) title)
       collect book)))

;; Try it out:

(get-author-title "Paul Graham" "ANSI Common Lisp")
