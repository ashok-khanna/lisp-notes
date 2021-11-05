;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FM-PLUGIN-TOOLS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/fm-objects.lisp,v 1.11 2010/07/22 09:38:06 edi Exp $

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :fm-plugin-tools)

;; unexport some symbols that were automatically exported by
;; PREPARE-FM-PLUGIN-TOOLS although we actually only need them
;; internally or not at all
(dolist (symbol '(+k40extn-version+
                  +k41extn-version+
                  +k50extn-version+
                  +k60extn-version+
                  +k70extn-version+
                  +k80extn-version+
                  +k-min-extn-version+
                  +k-max-extn-version+
                  +k-current-extn-version+
                  +k-bad-extn-version+
                  +k-fmxt-get-string+
                  +k-fmxt-idle+
                  +k-fmxt-init+
                  +k-fmxt-external+
                  +k-fmxt-shutdown+
                  +k-fmxt-do-app-preferences+
                  +k-fmxt-app-config-str+
                  +k-fmxt-options-str+
                  +k-fmxt-name-str+
                  +k-fmxt-developer+
                  +k-fmxt-mobile+
                  +k-fmxt-pro+
                  +k-fmxt-web+
                  +k-fmxt-runtime+
                  +k-fmxt-server+
                  +k-bad-alloc+
                  +k-unknown+
                  +k-no-err+
                  +k-color-channel-on+
                  +k-color-channel-off+
                  +k-do-not-enable+
                  +k-encoding-native+
                  +k-encoding-utf8+
                  +k-encoding-ascii-dos+
                  +k-encoding-ascii-windows+
                  +k-encoding-ascii-mac+
                  +k-encoding-iso-8859-1+
                  +k-encoding-iso-8859-2+
                  +k-encoding-iso-8859-3+
                  +k-encoding-iso-8859-4+
                  +k-encoding-iso-8859-5+
                  +k-encoding-iso-8859-6+
                  +k-encoding-iso-8859-7+
                  +k-encoding-iso-8859-8+
                  +k-encoding-iso-8859-9+
                  +k-encoding-iso-8859-15+
                  +k-encoding-arabic-mac+
                  +k-encoding-arabic-win+
                  +k-encoding-baltic-win+
                  +k-encoding-central-europe-mac+
                  +k-encoding-chinese-simp-mac+
                  +k-encoding-chinese-simp-win+
                  +k-encoding-chinese-trad-mac+
                  +k-encoding-chinese-trad-win+
                  +k-encoding-cyrillic-mac+
                  +k-encoding-cyrillic-win+
                  +k-encoding-eastern-europe-win+
                  +k-encoding-greek-mac+
                  +k-encoding-greek-win+
                  +k-encoding-hebrew-mac+
                  +k-encoding-hebrew-win+
                  +k-encoding-korean-johab+
                  +k-encoding-korean-mac+
                  +k-encoding-korean-win+
                  +k-encoding-shift-jis-mac+
                  +k-encoding-shift-jis-win+
                  +k-encoding-turkish-mac+
                  +k-encoding-turkish-win+))
  (unexport symbol :fm-plugin-tools))

(defclass fm-object ()
  ((pointer :initarg :pointer
            :reader pointer
            :documentation "The FLI pointer to the actual
FileMaker object.")
   (do-not-delete :initform nil
                  :initarg :do-not-delete
                  :reader do-not-delete
                  :documentation "If the value of this slot is
true, the corresponding C object won't be explicitly deleted by
Lisp code because it is expected to be deleted by FileMaker."))
  (:documentation "This is the base class for all classes
representing FileMaker objects.  It is basically just a proxy for
an FLI pointer and provides for automatic deletion of unused
objects."))

(defmethod initialize-instance :after ((fm-object fm-object) &rest initargs)
  "This :AFTER method makes sure that every FM-OBJECT object is
flagged for special action on garbage collection."
  (declare (ignore initargs))
  (flag-special-free-action fm-object))

(defun maybe-delete-fm-object (object)
  "This function will be executed with every object that is
flagged for special action on garbage collection.  We check that
it is of class FM-OBJECT, that we are supposed to delete it, and
that its pointer slot really contains an FLI pointer.  Then we
finally call the generic function FM-DELETE."
  (when (and (typep object 'fm-object)
             (not (do-not-delete object))
             (slot-boundp object 'pointer)
             (pointerp (pointer object)))
    ;; we actually have to define a method for each subclass
    (ignore-errors (fm-delete object))))

;; make sure MAYBE-DELETE-FM-OBJECT will do its work
(add-special-free-action 'maybe-delete-fm-object)

(defgeneric fm-delete (thing)
  (:documentation "This generic function will be called to delete
THING if it's an FM-OBJECT object about to be garbage-collected.  It
has to be specialized because the default method does nothing.")
  (:method (thing)))

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; will also be used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/fm-plugin-tools/")

;; this can't be defined earlier because of fli.lisp

(let ((exported-symbols-alist
        (loop for symbol being the external-symbols of :fm-plugin-tools
              collect (cons symbol
                            (concatenate 'string
                                         "#"
                                         (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))