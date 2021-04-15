;;; Lisp code for basic blog server. Does not do error handling if attempting to insert duplicate records


;; ---------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------

;; Load packages :: Hunchentoot for web server, Postmodern to interface with PostgresSQL
;; and Html-template to produce html web pages

(ql:quickload "hunchentoot")   
(ql:quickload :postmodern)    
(ql:quickload :html-template) 

(use-package :postmodern)

;; ---------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------

;; POSTMODERN CODE - Connect to database and create necessary functions to interface with it

(connect-toplevel "testdb" "testuser" "mypassword" "localhost")

(defun create-table ()
  (query "CREATE TABLE myBlogTable (title varchar(80) UNIQUE NOT NULL, body text);"))

(defun insert-record (title body) 
  (query (format nil "INSERT INTO myBlogTable VALUES ('~A', '~A');" title body)))

(defun read-record (title)
  (car
   (query (format nil "SELECT * FROM myBlogTable WHERE title = '~A';" title))))

(defun update-record (title body)
  (query (format nil "UPDATE myBlogTable SET body = '~A' where title = '~A';" body title)))

(defun delete-record (title)
  (query (format nil "DELETE FROM myBlogTable WHERE title = '~A';" title)))

(defun read-all ()
  (query "SELECT * FROM myBlogTable;"))

(create-table)
(insert-record "first-title" "First Blog Post")
(read-record "first-title")
(update-record "first-title" "Updated First Blog Post")

(insert-record "test-title" "test body")
(delete-record "test-title")
(read-all)



;; ---------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------

;; HUNCHENTOOT CODE - Initialise web server and set up dispatch handling for CRUD events

(defvar *acceptor*
    (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *acceptor*)

(setq hunchentoot:*dispatch-table*
      (list (hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
            (hunchentoot:create-regex-dispatcher "^/view/$"
                                                 'view-blog-post-page)
            (hunchentoot:create-regex-dispatcher "^/edit/$"
                                                 'edit-blog-post)
            (hunchentoot:create-regex-dispatcher "^/create/$"
                                                 'create-blog-post)))



;; ---------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------

;; HTML TEMPLATE CODE - Creating & Serving web pages

;; Helper function to create URL from title

(defun make-url-part (title)
  "Generate a url-part from a title. A url-part should only have
alphanumeric characters or dashes (in place of spaces)."
  (string-downcase
   (delete-if #'(lambda (x) (not (or (alphanumericp x) (char= #\- x))))
              (substitute #\- #\Space title))))

;; ---------------------------------------------------------------------------------------------

;; CREATE :: Create Blog Posts
;; Either GET a blank edit page or save the contents of the edited page via POST

(defun save-new-blog-post ()
    "Read POST data and modify blog post."
    (insert-record (hunchentoot:post-parameter "title") (hunchentoot:post-parameter "body"))
    (hunchentoot:redirect (format nil "http://localhost:4242/view/?~A"
          (make-url-part (hunchentoot:post-parameter "title")))))

(defun create-blog-post ()
  (cond ((eq (hunchentoot:request-method*) :GET)
   (with-output-to-string (stream)
     (html-template:fill-and-print-template #P"post-form.tmpl" nil :stream stream)))
  ((eq (hunchentoot:request-method*) :POST)
   (save-new-blog-post))))

;; ---------------------------------------------------------------------------------------------

;; READ :: Create index page and serve individual blog posts

(defun generate-blog-post-page (template blog-post)
  (with-output-to-string (stream) 
    (html-template:fill-and-print-template
       template
       (list :title (hunchentoot:query-string*)
             :body (nth 1 blog-post))
       :stream stream)))

(defun view-blog-post-page ()
  "Generate a page for viewing a blog post."
  (generate-blog-post-page #P"post.tmpl" (read-record (hunchentoot:query-string*))))

(defun generate-index-page ()
  "Generate the index page showing all the blog posts."
  (with-output-to-string (stream)
    (html-template:fill-and-print-template 
     #P"index.tmpl"
     (list :blog-posts 
           (loop for blog-post in (read-all)
              collect (list :title (nth 0 blog-post)
          :body (nth 1 blog-post)
          :url-part (make-url-part (nth 0 blog-post)))))
     :stream stream)))

;; ---------------------------------------------------------------------------------------------

;; UPDATE :: Edit Blog Posts
;; Either GET contents of blog via generate-blog-post-page, but with the edit HTML form template,
;; Or save the contents of the edited page via POST

(defun save-blog-post ()
    "Read POST data and modify blog post."
    (update-record (hunchentoot:post-parameter "title") (hunchentoot:post-parameter "body"))
    (hunchentoot:redirect (format nil "http://localhost:4242/view/?~A"
          (make-url-part (hunchentoot:post-parameter "title")))))

(defun edit-blog-post ()
  (cond ((eq (hunchentoot:request-method*) :GET)
         (generate-blog-post-page #P"post-form.tmpl" (read-record (hunchentoot:query-string*))))
        ((eq (hunchentoot:request-method*) :POST)
         (save-blog-post))))
     
;; ---------------------------------------------------------------------------------------------

;; DELETE ::  Try creating this yourself (not done here - but see above for interface functions)
