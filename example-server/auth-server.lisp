(ql:quickload '(
:hunchentoot
:cl-pass
:trivial-hashtable-serialize))

(setf user-map 
   (make-hash-table
:test #'equal
:size 10000
:rehash-size 10000
:rehash-threshold 0.8
:weakness nil
:synchronized nil))

;;main------
(setf web-server 
   (hunchentoot:start 
      (make-instance 'hunchentoot:easy-acceptor 
:address "127.0.0.1" 
:port 8000 
:document-root nil
:persistent-connections-p t
:read-timeout 3.0 
:write-timeout 3.0
:access-log-destination nil
:message-log-destination nil)))

;;index------
(hunchentoot:define-easy-handler (index :uri "/") (info)
    (setf (hunchentoot:content-type*) "text/html")
      (let ((the-user (hunchentoot:session-value :user)))
         (format nil 
"<h4>welcome-to-the-small-demo</h4><hr>
<p>info:~A</p>
<p>user:~A</p>
<a href=\"/sign-up\">click-here-to-sign-up</a><br>
<a href=\"/sign-in\">click-here-to-sign-in</a><br>
<a href=\"/sign-out\">click-here-to-sign-out</a><br>" info the-user)))

;;sign-up------
(hunchentoot:define-easy-handler (sign-up :uri "/sign-up") (info)
   (setf (hunchentoot:content-type*) "text/html")
      (format nil 
"<h4>welcome-to-sign-up</h4><hr>
<p>info:~A</p>
<a href=\"/\">click-here-to-index</a><br>
<form action=\"/sign-up-ok\" method=\"post\">
user:<br><input type=\"text\" name=\"user\" /><br>
pass:<br><input type=\"password\" name=\"pass\" /><br>
<input type=\"submit\" value=\"submit\" /></form>" info))

(hunchentoot:define-easy-handler (sign-up-ok :uri "/sign-up-ok") ()
   (setf (hunchentoot:content-type*) "text/plain")
      (let ((the-user   (hunchentoot:post-parameter "user"))
            (the-pass   (hunchentoot:post-parameter "pass")))
         (if   (and  (stringp the-user) (stringp the-pass)
                     (< 1 (length the-user) 200) (< 1 (length the-pass) 200))
               (if   (gethash the-user user-map)
                     (hunchentoot:redirect "/sign-up?info=has-been-used")
                     (progn
                        (setf (gethash the-user user-map) (cl-pass:hash the-pass))
                        (hunchentoot:redirect "/sign-in")))                   
               (hunchentoot:redirect "/sign-up?info=input-error"))))

;;sign-in------
(hunchentoot:define-easy-handler (sign-in :uri "/sign-in") (info)
   (setf (hunchentoot:content-type*) "text/html")
      (if   (hunchentoot:session-value :user)
            (hunchentoot:redirect "/?info=you-had-sign-in")
            (format nil 
"<h4>welcome-to-sign-in</h4><hr>
<p>info:~A</p>
<a href=\"/\">click-here-to-index</a><br>
<form action=\"/sign-in-ok\" method=\"post\">
user:<br><input type=\"text\" name=\"user\" /><br>
pass:<br><input type=\"password\" name=\"pass\" /><br>
<input type=\"submit\" value=\"submit\" /></form>" info)))

(hunchentoot:define-easy-handler (sign-in-ok :uri "/sign-in-ok") ()
   (setf (hunchentoot:content-type*) "text/plain")
      (let ((the-user   (hunchentoot:post-parameter "user"))
            (the-pass   (hunchentoot:post-parameter "pass")))
         (if   (and (stringp the-user) (stringp the-pass))
               (let ((pass-info (gethash the-user user-map)))
                  (if   (ignore-errors (cl-pass:check-password the-pass pass-info))
                        (progn
                           (hunchentoot:start-session)
                           (setf (hunchentoot:session-value :user) the-user)
                           (hunchentoot:redirect "/?info=you-had-sign-in"))
                        (hunchentoot:redirect "/sign-in?info=password-error")))
               (hunchentoot:redirect "/sign-in?info=input-error"))))

;;sign-out------
(hunchentoot:define-easy-handler (sign-out :uri "/sign-out") ()
   (setf (hunchentoot:content-type*) "text/plain")
      (hunchentoot:remove-session hunchentoot:*session*)
         (hunchentoot:redirect  "/?info=you-had-sign-out"))