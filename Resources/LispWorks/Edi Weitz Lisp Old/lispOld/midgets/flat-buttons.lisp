;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/flat-buttons.lisp,v 1.9 2013/08/23 08:04:12 edi Exp $

;;; Copyright (c) 2006-2013, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :midgets)

(defclass flat-button-pinboard-object (item-pinboard-object)
  ((callback :initarg :callback
             :initform nil
             :accessor flat-button-callback
             :documentation "The function that is called with the new
color if the user has selected one.  Or NIL for no callback.")
   (callback-type :initarg :callback-type
                  :initform :data
                  :accessor flat-button-callback-type
                  :documentation "Determines which arguments the
callback is called with.  Used as for the class CAPI:CALLBACKS."))
  (:documentation "The class that is responsible for the visual
appearance of the flat button on the screen and for its behaviour when
clicked.  Inherits from CAPI:ITEM-PINBOARD-OBJECT and thus also from
CAPI:ITEM."))

(defun check-callback-type (callback-type)
  "Checks that CALLBACK-TYPE is an acceptable value for the callback
type of a flat button and converts it into a `canonical'
representation as a list of keywords."
  (setq callback-type
        (case callback-type
          (:data '(:data))
          (:data-interface '(:data :interface))
          (:interface-data '(:interface :data))
          (:item '(:item))
          (:item-interface '(:item :interface))
          (:interface-item '(:interface :item))
          (:interface '(:interface))
          (:full '(:data :item :interface))
          (:none ())
          (otherwise callback-type)))
  (unless (listp callback-type)
    (error "Unknown callback type ~S." callback-type))
  (when (set-difference callback-type '(:data :item :interface) :test #'eq)
    (error "The list ~S contains unknown callback types." callback-type))
  (unless (equal callback-type (remove-duplicates callback-type))
    (error "The list ~S contains duplicate elements." callback-type))
  callback-type)

(defmethod initialize-instance :around ((self flat-button-pinboard-object)
                                        &rest other-args
                                        &key background foreground (callback-type :data))
  "This method adds two new initargs FOREGROUND and BACKGROUND to the
COLOR-BUTTON-PINBOARD-OBJECT class.  These are used to give the button
itself and its text colors which can't be changed later on."
  (setq callback-type (check-callback-type callback-type))
  (let (graphics-args)
    (when foreground
      (push foreground graphics-args)
      (push :foreground graphics-args))
    (when background
      (push background graphics-args)
      (push :background graphics-args))
    (apply #'call-next-method self
           :graphics-args graphics-args
           :callback-type callback-type
           (sans other-args :foreground :background :callback-type))))

(defmethod (setf flat-button-callback-type) :around (new-callback-type (self flat-button-pinboard-object))
  "First checks if the provided callback type is valid before the slot
is changed."
  (call-next-method (check-callback-type new-callback-type) self))

(defmethod (setf item-text) :after (new-text (self flat-button-pinboard-object))
  "Makes sure the object is redrawn when the text changes."
  (invalidate-pane-constraints self)
  (redraw-pinboard-object self))

(defmethod (setf item-data) :after (new-data (self flat-button-pinboard-object))
  "Makes sure the object is redrawn in case the text changes."
  (invalidate-pane-constraints self)
  (redraw-pinboard-object self))

(defmethod (setf item-print-function) :after (new-print-function (self flat-button-pinboard-object))
  "Makes sure the object is redrawn in case the text changes."
  (invalidate-pane-constraints self)
  (redraw-pinboard-object self))

(defmethod callback-arguments ((self flat-button-pinboard-object))
  "Returns a list of arguments for the button's callback depending on
its callback type."
  (loop for type-spec in (flat-button-callback-type self)
        collect (case type-spec
                  (:data (item-data self))
                  (:item self)
                  (:interface (element-interface self)))))

(defclass flat-button (simple-pinboard-layout)
  ((pinboard-object :initarg :pinboard-object
                    :reader flat-button-pinboard-object))
  (:documentation "This class is the user-visible implementation of a
single flat button.  It wraps a simple pinboard layout around a row
layout \(see below) which in turn contains the button's pinboard
object.")
  (:default-initargs
   :input-model '(((:button-1 :release) flat-button-button-1-release-callback))))

(defmethod flat-button-item ((self flat-button))
  "Returns the CAPI:ITEM object associated with the button."
  (flat-button-pinboard-object self))

(defmethod flat-button-callback ((self flat-button))
  "Returns the function which is called when the user presses the button."
  (flat-button-callback (flat-button-pinboard-object self)))

(defmethod (setf flat-button-callback) (new-callback (self flat-button))
  "Sets the function which is called when the user presses the button."
  (setf (flat-button-callback (flat-button-pinboard-object self)) new-callback))

(defmethod flat-button-callback-type ((self flat-button))
  "Returns the callback type of the button."
  (flat-button-callback-type (flat-button-pinboard-object self)))

(defmethod (setf flat-button-callback-type) (new-callback (self flat-button))
  "Sets the callback type of the button."
  (setf (flat-button-callback-type (flat-button-pinboard-object self)) new-callback))

(defclass string-row-layout (row-layout)
  ()
  (:documentation "A subclass of CAPI:ROW-LAYOUT that we only need in
order to specialize CAPI::MAXIMUM-STRING-LENGTH on it."))

(defmethod capi::maximum-string-length ((self string-row-layout))
  "Computes the string length of the row layout by delegating the
computation to its second child.  \(We assume that string row layouts
are only ever created by the method below.)"
  (capi::maximum-string-length (second (layout-description self))))

(defmethod initialize-instance :around ((self flat-button)
                                        &rest initargs &key background &allow-other-keys)
  "Makes sure that the initargs are split up correctly between the
FLAT-BUTTON object, the string row layout, and the underlying pinboard
object."
  (multiple-value-bind (pinboard-object-initargs other-initargs)
      (split-plist initargs
                   :callback :callback-type :data :text :print-function
                   :foreground :background)
    (let ((pinboard-object (apply #'make-instance 'flat-button-pinboard-object
                                  pinboard-object-initargs)))
      (multiple-value-bind (button-initargs layout-initargs)
          (split-plist other-initargs :help-key :font :visible-border :input-model :display-callback)
        (apply #'call-next-method self
               :child (apply #'make-instance 'string-row-layout
                             :description (list nil pinboard-object nil)
                             layout-initargs)
               :pinboard-object pinboard-object               
               :background background
               button-initargs)))))

(defun flat-button-button-1-release-callback (pane x y)
  "The callback that's called when the left mouse button is pressed
\(or actually released) on a flat button.  Calls the button's callback
with the right arguments if there is one."
  (declare (ignore x y))
  (let ((object (flat-button-pinboard-object pane)))
    (when-let (callback (flat-button-callback object))
      (apply callback (callback-arguments object)))))

(defclass flat-button-panel (simple-layout)
  ((buttons :initarg :buttons
            :reader flat-button-panel-buttons
            :documentation "The list of the flat buttons in the panel.")
   (test-function :initarg :test-function
                  :initform 'eql
                  :reader flat-button-panel-test-function
                  :documentation "The function which is used to
compare two items of the panel."))
  (:documentation "The class which implements flat button panels.
Technically, this is a simple layout, but you should for all practical
purposes just treat it like a simple pane."))

(defmethod flat-button-panel-items ((self flat-button-panel))
  "Returns the list of items associated with the buttons in the panel."
  (mapcar #'item-data (flat-button-panel-buttons self)))

(defmethod flat-button-panel-button ((self flat-button-panel) item)
  "Returns the button from the flat button panel SELF that's
associated with the item ITEM.  Will return NIL if no such button can
be found."
  (find item (flat-button-panel-buttons self)
        :test (flat-button-panel-test-function self)
        :key 'item-data))

(defmethod initialize-instance :around ((self flat-button-panel)
                                        &rest initargs
                                        &key items (print-function 'princ-to-string)
                                        buttons button-args button-args-function
                                        callback (callback-type :data) help-keys
                                        (layout-class 'column-layout)
                                        (layout-args (when (or (subtypep layout-class 'row-layout)
                                                               (subtypep layout-class 'column-layout))
                                                       '(:uniform-size-p t))))
  "Adds new initargs to the FLAT-BUTTON-PANEL object.  Sets up the
layout to be used for the panel and creates the actual flat buttons
which will be put into the layout.

ITEMS is a list of arbitrary Lisp objects representing the buttons.
PRINT-FUNCTION should be a function of one argument which will be
called for each item and should return its text.

BUTTON-ARGS is a property list with additional initargs for the
buttons that are going to be created.  Or, if you want more
fine-grained control, you can use BUTTON-ARGS-FUNCTION to provide
different initargs per item.

HELP-KEYS, if provided, should be a list of help keys in one-to-one
correspondence with the list ITEMS.

CALLBACK is the function that will be called when one of the buttons
is clicked.  It will be called with the arguments determined by
CALLBACK-TYPE \(used as in CAPI:CALLBACKS).

You can also simply provide a list BUTTONS of flat buttons.  In this
case, all the initargs mentioned above will be ignored.

LAYOUT-CLASS and LAYOUT-ARGS determine how the button panel will be
layed out.  This works like with CAPI's stock button panels."
  (let* ((buttons (or buttons
                      (loop for item in items
                            for help-key in (or help-keys (make-list (length items)))
                            collect (apply #'make-instance 'flat-button
                                           :data item
                                           :callback callback
                                           :callback-type callback-type
                                           :help-key help-key
                                           :print-function print-function
                                           (or button-args
                                               (and button-args-function
                                                    (funcall button-args-function item)))))))
         (layout (apply #'make-instance layout-class
                        :description buttons
                        layout-args)))
    (apply #'call-next-method self
           :child layout
           :buttons buttons
           (sans initargs
                 :layout-class :layout-args
                 :items :print-function :buttons :button-args :button-args-function
                 :callback :callback-type :help-keys))))
    
