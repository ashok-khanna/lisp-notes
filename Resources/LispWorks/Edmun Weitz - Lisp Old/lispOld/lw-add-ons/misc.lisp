;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LW-ADD-ONS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/lw-add-ons/misc.lisp,v 1.31 2015/03/06 12:54:25 edi Exp $

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

(in-package :lw-add-ons)

(defmacro ignore-errors* (&body body)
  "Like IGNORE-ERRORS, but also binds *BREAK-ON-SIGNALS* to
*LW-ADD-ONS-BREAK-ON-SIGNALS* so that LW-ADD-ONS usually doesn't
interfer with debugging."
  `(let ((*break-on-signals* *lw-add-ons-break-on-signals*))
     (ignore-errors ,@body)))

(defun browse-anchored-uri (uri)
  "Show the URI URI in a browser."
  ;; workaround because older versions of LispWorks's HWEB:BROWSE
  ;; function swallow the fragment part of the URI - based on an
  ;; idea by Nick Levine
  #-(or :lispworks5 :lispworks6)
  (let ((temp-file (make-temp-file nil "html")))
    (push temp-file *temp-files*)
    (with-open-file (out temp-file
                         :direction :output
                         :if-exists :supersede)
      (format out "<html><head><meta http-equiv=refresh content=\"0;url=~A\"></head></html>"
              uri))
    (hweb:browse (namestring temp-file)))
  #+(or :lispworks5 :lispworks6)
  (hweb:browse uri))

(defun start-swank-server ()
  "Starts Swank so you can control LispWorks from Emacs via
SLIME.  Note that this might cause conflicts with the LW IDE."
  (unless *swank-loader-pathname*
    (error "You need to specify *SWANK-LOADER-PATHNAME*."))
  (unless *swank-started-p*
    (load *swank-loader-pathname*)
    (setq *swank-started-p* t))
  (funcall (find-symbol "CREATE-SERVER" :swank) :dont-close t))

(defun starts-with-p (string prefix &key (test #'char-equal))
  "Whether the string STRING starts with PREFIX."
  (let ((mismatch (mismatch string prefix :test test)))
    (or (null mismatch)
        (= mismatch (length prefix)))))

(defun tile-windows-vertically (screen)
  "Tiles windows vertically if in MDI mode."
  (let ((podium
         (capi:locate-interface 'lispworks-tools::lispworks-podium
                                :screen screen)))
    (when (and podium
               (typep podium 'lispworks-tools::lispworks-win32-mdi-podium))
      (capi:execute-with-interface podium 'capi::windows-menu-callback 
                                   podium :tile-vertically))))

(defun open-editor-and-tile-windows-vertically (screen)
  "Opens an editor if necessary and tiles windows vertically."
  (capi:find-interface 'lw-tools:editor)
  (tile-windows-vertically screen))

(defun gui-inspect (object)
  "Opens an IDE inspector to inspect the object OBJECT."
  (capi:find-interface 'lw-tools:inspector :object object))

(defun format-object-for-apropos (object)
  "Returns a string representing OBJECT which isn't \(much) longer
than *APROPOS-STRING-LENGTH*."
  (with-standard-io-syntax
    (let* ((*print-circle* t)
           (*print-readably* nil)
           (*print-length* *apropos-print-length*)
           (*print-level* *apropos-print-level*)
           (string (format nil "~S" object)))
      (cond ((<= (length string) *apropos-max-string-length*)
             string)
            (t (format nil "~A ..."
                       (subseq string 0 *apropos-max-string-length*)))))))

(defun source-can-be-found (symbol)
  "Whether a source location for the symbol SYMBOL is known."
  (remove :unknown
          (append (dspec:find-name-locations dspec:*dspec-classes* symbol)
                  (dspec:find-name-locations '(function) `(setf ,symbol)))
          :test #'eq
          :key #'second))

(defun documentation-uri (symbol)
  "Returns the documentation URI for the symbol SYMBOL if it exists."
  ;; see "Meta Documentation" command
  (let* ((symbol-string (and symbol
                             (format nil "~:[~;:~]~A"
                                     (keywordp symbol)
                                     (symbol-name symbol)))))
    (and symbol-string (doc-entry symbol-string))))

(setf (sys:product-registry-path :lw-add-ons)
      *product-registry-path*)

(defun get-apropos-user-preference (key default)
  (multiple-value-bind (value present)
      (user-preference "Apropos Dialog Settings" key
                       :product :lw-add-ons)
    (if present value default)))

(defun set-apropos-user-preferences (&rest args)
  (loop for (key value . nil) on args by #'cddr
        do (setf (user-preference "Apropos Dialog Settings" key
                                  :product :lw-add-ons)
                 value)))

(define-action "When quitting image" "Delete temporary files"
  (lambda ()
    (loop for file in *temp-files*
          do (ignore-errors* (delete-file file)))))

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>

(defvar *hyperdoc-base-uri* "http://weitz.de/lw-add-ons/")

(let ((exported-symbols-alist
       (loop for symbol in '(*completion-match-function*
                             *show-doc-string-when-showing-arglist*
                             *max-completions-to-show*
                             *insert-right-parenthesis-if-no-args*
                             *mop-page*
                             *translate-asdf-systems*
                             *listener-shortcuts*
                             *max-info-length*
                             *swank-loader-pathname*
                             start-swank-server)
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
               
