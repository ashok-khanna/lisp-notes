(in-package :cl-wbxml)

(defun test1-out (&optional (file "/tmp/bar.txt"))
  (with-open-file (out file :direction :output
                       :if-exists :supersede)
    (setq out (make-flexi-stream out :external-format :utf-8))
    (write-sequence '(1 1 #x6a #x12 #\a #\b #\c 0 #\Space #\E #\n #\t #\e #\r #\Space #\n
                      #\a #\m #\e #\: #\Space 0 #x47 #xc5 9 #x83 0 5 1 #x88 6
                      #x86 8 3 #\x #\y #\z 0 #x85 3 #\/ #\s 0 1 #x83 4
                      #x86 7 #xa 3 #\N 0 1 1 1)
                    out)))

(defun test2-out (&optional (file "/tmp/bar2.txt"))
  (with-open-file (out file :direction :output
                       :if-exists :supersede)
    (setq out (make-flexi-stream out :external-format :ascii))
    (write-sequence '(1 1 3 0 #x47 #x46 3 #\Space #\X #\Space #\& #\Space #\Y
                      0 5 3 #\Space #\X 0 2 #x81 #x20 3 #\= 0 2 #x81 #x20 3
                      #\1 #\Space 0 1 1)
                    out)))

(defun test1 (&optional (file "/tmp/bar.txt"))
  (with-open-file (in file)
    (parse-wbxml in (make-instance 'xmls-handler)
                 :tag-tokens
                 '((0 . ((5 . "CARD")
                         (6 . "INPUT")
                         (7 . "XYZ")
                         (8 . "DO"))))
                 :attr-tokens
                 '((0 . ((5 . ("STYLE" . "LIST"))
                         (6 . ("TYPE"))
                         (7 . ("TYPE" . "TEXT"))
                         (8 . ("URL" . "http://"))
                         (9 . ("NAME"))
                         (10 . ("KEY"))
                         (#x85 . ".org")
                         (#x86 . "ACCEPT")))))))

(defun test1* (&optional (file "/tmp/bar.txt"))
    (parse-wbxml file (make-instance 'xmls-handler)
                 :tag-tokens
                 '((0 . ((5 . "CARD")
                         (6 . "INPUT")
                         (7 . "XYZ")
                         (8 . "DO"))))
                 :attr-tokens
                 '((0 . ((5 . ("STYLE" . "LIST"))
                         (6 . ("TYPE"))
                         (7 . ("TYPE" . "TEXT"))
                         (8 . ("URL" . "http://"))
                         (9 . ("NAME"))
                         (10 . ("KEY"))
                         (#x85 . ".org")
                         (#x86 . "ACCEPT"))))))

(defun test2 (&optional (file "/tmp/bar2.txt"))
  (with-open-file (in file)
    (parse-wbxml in (make-instance 'xmls-handler)
                 :tag-tokens
                 '((0 . ((5 . "BR")
                         (6 . "CARD")
                         (7 . "XYZ")))))))

(defun test1* (&optional (file "/tmp/barx.txt"))
  (with-open-file (out file :direction :output
                       :if-exists :supersede
                       :external-format '(:latin-1 :eol-style :lf))
    (unparse-wbxml (test1) out
                 :tag-tokens
                 '((0 . ((5 . "CARD")
                         (6 . "INPUT")
                         (7 . "XYZ")
                         (8 . "DO"))))
                 :attr-tokens
                 '((0 . ((5 . ("STYLE" . "LIST"))
                         (6 . ("TYPE"))
                         (7 . ("TYPE" . "TEXT"))
                         (8 . ("URL" . "http://"))
                         (9 . ("NAME"))
                         (10 . ("KEY"))
                         (#x85 . ".org")
                         (#x86 . "ACCEPT"))))))
  (test1 file))

(defun testx ()
    (unparse-wbxml (test1) t
                 :tag-tokens
                 '((0 . ((5 . "CARD")
                         (6 . "INPUT")
                         (7 . "XYZ")
                         (8 . "DO"))))
                 :attr-tokens
                 '((0 . ((5 . ("STYLE" . "LIST"))
                         (6 . ("TYPE"))
                         (7 . ("TYPE" . "TEXT"))
                         (8 . ("URL" . "http://"))
                         (9 . ("NAME"))
                         (10 . ("KEY"))
                         (#x85 . ".org")
                         (#x86 . "ACCEPT"))))))

(defun test2* (&optional (file "/tmp/barx2.txt"))
  (with-open-file (out file :direction :output
                       :if-exists :supersede
                       :external-format '(:latin-1 :eol-style :lf))
    (unparse-wbxml (test2) out
                   :tag-tokens
                   '((0 . ((5 . "BR")
                           (6 . "CARD")
                           (7 . "XYZ"))))))
  (test2 file))
                   
(defun general (&optional (file (capi:prompt-for-file "FOO:")))
  (let* ((doc
          (with-open-file (in file :element-type 'octet)
            (print (parse-wbxml in (make-xmls-handler)))))
         (tmp "/tmp/test.file"))
    (with-open-file (out tmp :element-type 'octet :direction :output :if-exists :supersede)
      (unparse-wbxml doc out :publicid "-//SYNCML//DTD SyncML 1.1//EN"))
    (with-open-file (in tmp :element-type 'octet)
      (parse-wbxml in (make-xmls-handler)))
    (unparse-wbxml doc t :publicid "-//SYNCML//DTD SyncML 1.1//EN")))
                   
(defun general (&optional (file (capi:prompt-for-file "FOO:")))
  (with-open-file (in file :element-type 'octet)
    (parse-wbxml in (make-xmls-handler))))
