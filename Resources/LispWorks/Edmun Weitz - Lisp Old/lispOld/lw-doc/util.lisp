;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-DOC; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-doc/util.lisp,v 1.13 2015/05/29 18:21:33 edi Exp $

;;; Copyright (c) 2005-2015, Dr. Edmund Weitz. All rights reserved.

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

(in-package :lw-doc)

(defun file-contents (pathname &rest open-arguments)
  "Returns the whole contents of the file denoted by PATHNAME as
one sequence with the corresponding element type."
  (with-open-stream (stream (apply #'open pathname
                                   :direction :input
                                   open-arguments))
    (when stream
      (let ((buffer (make-array (file-length stream)
                                :element-type (stream-element-type stream))))
        (cond ((= (read-sequence buffer stream) (length buffer))
               buffer)
              (t (error "Incomplete READ-SEQUENCE from ~S."
                        (pathname stream))))))))

(defun first-two-equal (list-1 list-2)
  "Tests whether the first two elements of LIST-1 and LIST-2 are
pairwise EQUAL."
  (and (equal (first list-1) (first list-2))
       (equal (second list-1) (second list-2))))

(defun remove-html-entities (string)
  "Replaces \(some) HTML entities in STRING with the characters they denote."
  (flet ((un-html (target-string start end match-start match-end reg-starts reg-ends)
           (declare (ignore start end reg-starts reg-ends))
           (let ((match (subseq target-string (1+ match-start) (1- match-end))))
             (let ((entity (assoc match +html-entities+ :test #'string=)))
               (when entity
                 (return-from un-html (cdr entity))))
             (let ((char-code (parse-integer match :start 1 :junk-allowed t)))
               (when char-code
                 (return-from un-html (string (code-char char-code)))))
             (subseq target-string match-start match-end))))
    (regex-replace-all "&[^;]+;" string #'un-html)))

(defun normalize-char (char)
  "Returns a downcased version of CHAR if CHAR is an alphabetic
\(ASCII) character and #\* otherwise."
  (cond ((char<= #\a char #\z) char)
        ((char<= #\A char #\Z) (char-downcase char))
        (t #\*)))

(declaim (inline nsubseq))
(defun nsubseq (sequence start &optional (end (length sequence)))
  "Like SUBSEQ but the result shares structure with SEQUENCE."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defun make-file-name (&optional char (folder-name "permuted-index"))
  "Creates a pathname for the index file for the character CHAR
or for the start page if CHAR is NIL.  Depends on the value of
*TARGET-DIR*."
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (list :relative folder-name)
                   :type "html"
                   :name (format nil "~@[permuted-~]index~@[-~A~]"
                                 (case char
                                   ((nil) nil)
                                   (#\* "non-alphabetic")
                                   (otherwise char))
                                 nil))
    *target-dir*)))

(defun escape-and-fill-spaces (string &optional start (end (length string)))
  "Replaces some characters in the substring of STRING denoted by
START and END with their XML character entities, also replaces
spaces with `&nbsp;'."
  (regex-replace-all " "
                     (escape-string (nsubseq string start end))
                     "&nbsp;"))

