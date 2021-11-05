;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGEX-PLUGIN; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/regex-plugin/configuration.lisp,v 1.11 2008/01/07 07:36:02 edi Exp $

;;; Copyright (c) 2006-2008, Dr. Jens Teich and Dr. Edmund Weitz.  All rights reserved.

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

(in-package :regex-plugin)

(capi:define-interface config-interface ()
  ()
  (:panes
   (cache-state-button
    capi:check-button
    :text "Cache regular expressions"
    :selected *cache-regexes-p*
    :callback-type :none
    :selection-callback (lambda ()
                          (setq *cache-regexes-p* t)
                          ;; store new value in registry
                          (store-config-values))
    :retract-callback (lambda ()
                        (setq *cache-regexes-p* nil)
                        ;; store new value in registry
                        (store-config-values))
    :help-key "Whether regular expressions should be cached instead of being re-computed for each field.
You should only un-check this if you have lots of different regular expressions.")
   (cache-empty-button
    capi:push-button
    :reader config-interface-cache-empty-button
    :title ""
    :title-position :right
    :text "Empty cache"
    :enabled *cache-regexes-p*
    :callback-type :none
    :callback (lambda ()
                ;; clear cache
                (clrhash *regex-cache*)
                ;; update information about cache size (now zero)
                (setf (capi:titled-object-title cache-empty-button)
                      (show-regex-cache-size))
                ;; GC just in case
                (mark-and-sweep 3))
    :help-key "Removes all cached regular expressions from the cache, so they have to be re-computed the next time they are used.")
   (ok-button
    capi:push-button
    :reader config-interface-ok-button
    :default-p t
    :text "   OK   "
    :callback-type :none
    :callback (lambda ()
                (capi:abort-dialog))
    :help-key "Close this dialog.")
   (description-pane
    capi:title-pane
    :text "(Move your mouse over the buttons for more detailed information.)"))
  (:layouts
   (dummy-layout
    capi:column-layout
    '(" ")
    :visible-max-height '(:character 1)
    :visible-max-width :screen-width)
   (ok-layout
    capi:row-layout
    '(dummy-layout ok-button))
   (main-layout
    capi:column-layout
    '(cache-state-button cache-empty-button nil description-pane nil ok-layout)
    :gap 10))
  (:documentation "The interface class used to create the
configuration dialog for this plug-in.")
  (:default-initargs
   :layout 'main-layout
   :auto-menus nil
   :title *plugin-name*
   :internal-border 10
   :create-callback (lambda (interface)
                      (setf (capi:titled-object-title
                             (config-interface-cache-empty-button interface))
                            (show-regex-cache-size)))
   ;; show tooltips
   :help-callback (lambda (interface pane type key)
                    (declare (ignore interface pane))
                    (when (and (stringp key)
                               (eq type :tooltip))
                      key))
   ;; hook to catch and log errors provided by FM-PLUGIN-TOOLS
   :top-level-hook #'top-level-hook))

(defun handle-configuration ()
  "The function to handle plug-in configuration.  Will be called
if the user presses the `Configure' button in FileMaker's
preferences dialog."
  (let ((config-interface (make-instance 'config-interface)))
    (setf (capi:pane-initial-focus config-interface)
          (config-interface-ok-button config-interface))
    (capi:display-dialog config-interface)))