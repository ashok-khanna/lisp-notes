;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/interface.lisp,v 1.12 2011/02/11 19:56:05 edi Exp $

;;; Copyright (c) 2006-2011, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :starter-pack)

(defclass lib-button (check-button)
  ((lib :initarg :lib
        :reader lib
        :documentation "The LIB object belonging to this button."))
  (:documentation "A LIB-BUTTON is like a CAPI check button, but
it contains an additional slot for the library it denotes."))

(defmethod set-button-state ((lib-button lib-button))
  "Updates the state of the CAPI check button LIB-BUTTON
\(enabled and/or selected) according to whether the corresponding
library is selected or checked."
  (let ((lib (lib lib-button)))
    (apply-in-pane-process lib-button
                           (lambda ()
                             (setf (button-enabled lib-button)
                                   (not (required lib))
                                   (button-selected lib-button)
                                   (or (checked lib) (required lib)))))))

(defun update-button-states ()
  "Re-computes all dependencies and updates all button states."
  (compute-dependencies)
  (dolist (lib-button *lib-buttons*)
    (set-button-state lib-button)))

(defun make-lib-button (lib)
  "Creates and returns a LIB-BUTTON corresponding to the LIB
object LIB."
  (make-instance 'lib-button
                 :lib lib
                 :help-key (description lib)
                 :callback-type :none
                 :text (string (name lib))
                 :selection-callback (lambda ()
                                       (setf (checked lib) t)
                                       (update-button-states))
                 :retract-callback (lambda ()
                                     (setf (checked lib) nil)
                                     (update-button-states))))

(defun make-lib-button-panel ()
  "Creates and returns a CAPI grid layout which contains one
LIB-BUTTON per *LIBS* element.  Also sets the special variable
*LIB-BUTTONS*."
  (setq *lib-buttons* (loop for lib in *libs*
                            collect (make-lib-button lib)))
  (make-instance 'grid-layout
                 :columns 4
                 :orientation :column
                 :description *lib-buttons*))

(defun create-interface ()
  "Creates and returns the graphical interface of the
application."
  (let* ((button-panel (make-lib-button-panel))
         ;; a fixed title pane showing the library directory
         (target-pane (make-instance 'title-pane
                                     :title (format nil "Target: ~A" (namestring (lib-dir)))))
         ;; a title pane showing the current status - see SET-STATUS
         (status-pane (make-instance 'title-pane
                                     :title "Status: Idle."))
         ;; a push button to check all LIB-BUTTONs
         (check-all-button (make-instance 'push-button
                                          :text "Check all"
                                          :help-key "Select all libraries."
                                          :callback-type :none
                                          :callback (lambda ()
                                                      (dolist (lib *libs*)
                                                        (setf (checked lib) t))
                                                      (update-button-states))))
         ;; a push button to uncheck all LIB-BUTTONs
         (uncheck-all-button (make-instance 'push-button
                                            :text "Uncheck all"
                                            :help-key "Deselect all libraries."
                                            :callback-type :none
                                            :callback (lambda ()
                                                        (dolist (lib *libs*)
                                                          (setf (checked lib) nil))
                                                        (update-button-states))))
         ;; the push button to start the download and installation process
         (start-button (make-instance 'push-button
                                      :text "Start"
                                      :help-key "Download and install selected libraries."
                                      :callback-type :item-interface
                                      :callback #'maybe-do-the-work))
         (button-layout (make-instance 'row-layout
                                       :description (list check-all-button uncheck-all-button
                                                          nil start-button)))
         (layout (make-instance 'column-layout
                                :gap 5
                                :description (list target-pane status-pane
                                                   button-panel button-layout))))
    ;; set special variable which will be used by SET-STATUS
    (setq *status-pane* status-pane)
    (make-instance 'interface
                   :layout layout
                   :title (format nil "Lisp Starter Pack ~A" (starter-pack-version-string))
                   :internal-border 5
                   ;; set correct button states initially
                   :create-callback (lambda (interface)
                                      (declare (ignore interface))
                                      (update-button-states))
                   ;; the callback used to show the tooltips
                   :help-callback (lambda (interface pane type key)
                                    (declare (ignore interface pane))
                                    (when (and (stringp key)
                                               (eq type :tooltip))
                                      key)))))

(defun set-status (&optional (format-string "Idle.") &rest format-args)
  "Updates the status pane of the graphical user interface.
Arguments are used as with FORMAT.  If no arguments are provided,
the text \"Idle.\" is shown."
  (apply-in-pane-process *status-pane*
                         #'(setf titled-pane-title)
                         (format nil "Status: ~?" format-string format-args)
                         *status-pane*))
