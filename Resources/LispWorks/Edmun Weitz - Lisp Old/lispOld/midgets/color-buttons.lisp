;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/color-buttons.lisp,v 1.11 2013/08/23 08:04:12 edi Exp $

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

(defclass color-button-pinboard-object (drawn-pinboard-object)
  ((color :initarg :color
          :initform (random-color)
          :accessor color-button-color
          :documentation "The color that this button represents.")
   (callback :initarg :callback
             :initform nil
             :accessor color-button-callback
             :documentation "The function that is called with the new
color if the user has selected one.  Or NIL for no callback.")
   (prompt-text :initarg :prompt-text
                :initform "Please select a color:"
                :accessor color-button-prompt-text
                :documentation "The title for the color chooser
dialog.  Ignored on Windows.")
   (prompt-args :initarg :prompt-args
                :initform nil
                :accessor color-button-prompt-args
                :documentation "Additional arguments that can be given
to CAPI:PROMPT-FOR-COLOR if needed."))
  (:documentation "The class that is responsible for the visual
appearance of the color button on the screen and for its behaviour
when clicked.  Inherits from CAPI:DRAWN-PINBOARD-OBJECT.")
  (:default-initargs
   :display-callback 'draw-color-button
   ;; see INITIALIZE-INSTANCE method for VISIBLE-MIN-WIDTH and
   ;; VISIBLE-MIN-HEIGHT
   :visible-max-width t
   :visible-max-height t))

(defmethod initialize-instance :after ((self color-button-pinboard-object) &rest initargs)
  "This method adds two new initargs WIDTH and HEIGHT to the
COLOR-BUTTON-PINBOARD-OBJECT class.  These are used to give the button
a fixed size which can't be changed later on."
  ;; note that this way of adding more initargs is ANSI-compatible due
  ;; to section 7.1.2 of the spec
  (lw:when-let (width (getf initargs :width))
     (capi:set-geometric-hint self :visible-min-width width))
  (lw:when-let (height (getf initargs :height))
     (capi:set-geometric-hint self :visible-min-height height)))

(defmethod (setf color-button-color) :after (new-color (self color-button-pinboard-object))
  "When the contents of the COLOR button changes, we have to redraw
the pinboard object and call the callback if there is one."
  (capi:redraw-pinboard-object self)
  (when-let (callback (color-button-callback self))
    (funcall callback new-color)))

(defun draw-color-button (pane self x y width height)
  "The display callback for the color button pinboard objects.  Simply
draws a filled rectangle with the right size and color."
  (gp:draw-rectangle pane x y width height :filled t :foreground (color-button-color self)))

(defclass color-button (simple-pinboard-layout titled-object)
  ((item :initarg :item
         :reader color-button-item
         :documentation "An `item' associated with the color button.
Can be an arbitrary Lisp object.  This is mostly useful in connection
with panels - see below.  Note that this is /not/ related to CAPI:ITEM
objects."))
  (:documentation "This class is the user-visible implementation of a
single color button.  It wraps a simple pinboard layout around the
button's pinboard object and also inherits from the CAPI:TITLED-OBJECT
mixin.")
  (:default-initargs
   :width 20
   :height 20
   :visible-border t
   :input-model '(((:button-1 :release) color-button-button-1-release-callback))))

(defmethod color-button-pinboard-object ((self color-button))
  "A reader which returns the underlying pinboard object from a color
button.  Only used internally."
  (first (layout-description self)))

(defmethod color-button-color ((self color-button))
  "Returns the color the color button represents."
  (color-button-color (color-button-pinboard-object self)))

(defmethod (setf color-button-color) (new-color (self color-button))
  "Sets the color the color button represents.  This will also result
in the button being redrawn with the new color."
  (setf (color-button-color (color-button-pinboard-object self)) new-color))

(defmethod color-button-callback ((self color-button))
  "Returns the function which is called when the user changes the
color of the button via the CAPI:PROMPT-FOR-COLOR function."
  (color-button-callback (color-button-pinboard-object self)))

(defmethod (setf color-button-callback) (new-callback (self color-button))
  "Sets the function which is called when the user changes the color
of the button via the CAPI:PROMPT-FOR-COLOR function.  Should be NIL
or a function of one argument, the new color."
  (setf (color-button-callback (color-button-pinboard-object self)) new-callback))

(defmethod color-button-prompt-text ((self color-button))
  "Returns the title for the color chooser dialog.  Meaningless on Windows."
  (color-button-prompt-text (color-button-pinboard-object self)))

(defmethod (setf color-button-prompt-text) (new-prompt-text (self color-button))
  "Sets the title for the color chooser dialog.  Ignored on Windows."
  (setf (color-button-prompt-text (color-button-pinboard-object self)) new-prompt-text))

(defmethod color-button-prompt-args ((self color-button))
  "Returns additional arguments for the CAPI:PROMPT-FOR-COLOR function."
  (color-button-prompt-args (color-button-pinboard-object self)))

(defmethod (setf color-button-prompt-args) (new-prompt-args (self color-button))
  "Sets additional arguments for the CAPI:PROMPT-FOR-COLOR function."
  (setf (color-button-prompt-args (color-button-pinboard-object self)) new-prompt-args))

(defmethod initialize-instance :around ((self color-button)
                                        &rest initargs
                                        &key color callback prompt-text prompt-args width height)
  (declare (ignore color callback prompt-text prompt-args width height))
  "Makes sure that the initargs are split up correctly between the
COLOR-BUTTON object and the underlying pinboard object."
  (multiple-value-bind (pinboard-object-initargs button-initargs)
      (split-plist initargs :color :callback :prompt-text :prompt-args :width :height)
    (apply #'call-next-method self
           :child (apply #'make-instance 'color-button-pinboard-object pinboard-object-initargs)
           button-initargs)))

(defun color-button-button-1-release-callback (pane x y)
  "The callback that's called when the left mouse button is pressed
\(or actually released) on a color button.

Calls CAPI:PROMPT-FOR-COLOR and then the button's callback if there is
one.  Also changes the button's color to the new color returned from
CAPI:PROMPT-FOR-COLOR if the dialog wasn't cancelled."
  (when-let (object (pinboard-object-at-position pane x y))
    (when (typep object 'color-button-pinboard-object)
      (multiple-value-bind (new-color successp)            
          (apply #'prompt-for-color (color-button-prompt-text object)
                 :color (color-button-color object)
                 (color-button-prompt-args object))
        (when successp
          (setf (color-button-color object) new-color))))))

(defclass color-button-panel (simple-layout)
  ((buttons :initarg :buttons
            :reader color-button-panel-buttons
            :documentation "The list of the color buttons in the panel.")
   (test-function :initarg :test-function
                  :initform 'eql
                  :reader color-button-panel-test-function
                  :documentation "The function which is used to
compare two items of the panel."))
  (:documentation "The class which implements color button panels.
Technically, this is a simple layout, but you should for all practical
purposes just treat it like a simple pane."))

(defmethod color-button-panel-items ((self color-button-panel))
  "Returns the list of items associated with the buttons in the panel."
  (mapcar #'color-button-item (color-button-panel-buttons self)))

(defmethod color-button-panel-button ((self color-button-panel) item)
  "Returns the button from the color button panel SELF that's
associated with the item ITEM.  Will return NIL if no such button can
be found."
  (find item (color-button-panel-buttons self)
        :test (color-button-panel-test-function self)
        :key 'color-button-item))

(defmethod color-button-panel-item-color ((self color-button-panel) item)
  "Returns the color of the button from the color button panel SELF
that's associated with the item ITEM.  Will return NIL if no such
button can be found."
  (when-let (button (color-button-panel-button self item))
    (color-button-color button)))

(defmethod (setf color-button-panel-item-color) (new-color (self color-button-panel) item)
  "Sets the color of the button from the color button panel SELF
that's associated with the item ITEM.  Does nothing if no such button
can be found."
  (when-let (button (color-button-panel-button self item))
    (setf (color-button-color button) new-color))
  new-color)

(defmethod initialize-instance :around ((self color-button-panel)
                                        &rest initargs
                                        &key buttons items callback help-keys
                                        (layout-class 'column-layout) layout-args
                                        color-function (title-function 'princ-to-string)
                                        button-args button-args-function)
  "Adds new initargs to the COLOR-BUTTON-PANEL object.  Sets up the
layout to be used for the panel and creates the actual color buttons
which will be put into the layout.

ITEMS is a list of arbitrary Lisp objects representing the buttons.
COLOR-FUNCTION should be a function of one argument which will be
called for each item and should return its button's initial color.
Likewise, TITLE-FUNCTION is supposed to provide a title for each item
\(i.e. button).

BUTTON-ARGS is a property list with additional initargs for the
buttons that are going to be created.  Or, if you want more
fine-grained control, you can use BUTTON-ARGS-FUNCTION to provide
different initargs per item.

HELP-KEYS, if provided, should be a list of help keys in one-to-one
correspondence with the list ITEMS.

CALLBACK should be a function of two arguments which will be called
with the item and the new color whenever the user has clicked on a
button and selected a new color.

You can also simply provide a list BUTTONS of color buttons.  In this
case, all the initargs mentioned above will be ignored.

LAYOUT-CLASS and LAYOUT-ARGS determine how the button panel will be
layed out.  This works like with CAPI's stock button panels."
  (when button-args
    ;; yes, it's pretty dumb to provide both arguments at once...
    (setq button-args-function (constantly button-args)))
  (let* ((buttons (or buttons
                      (loop for item in items
                            for help-key in (or help-keys (make-list (length items)))
                            for help-key-args = (and help-key
                                                     (list :help-key help-key))
                            for callback-args = (and callback
                                                     (list :callback
                                                           (let ((item item))
                                                             (lambda (new-color)
                                                               (funcall callback item new-color)))))
                            for color = (and color-function
                                             (funcall color-function item))
                            for color-args = (and color
                                                  (list :color color))
                            for title = (and title-function
                                             (funcall title-function item))
                            for title-args = (and title
                                                  (list :title title))
                            for button-args = (and button-args-function
                                                   (funcall button-args-function item))
                            ;; try to find a sensible title position
                            ;; if none was provided (and if there /is/
                            ;; a title)
                            for title-position = (and (or title (getf button-args :title))
                                                      (not (getf button-args :title-position))
                                                      (case layout-class
                                                        (column-layout :left)))
                            for title-position-args = (and title-position
                                                           (list :title-position title-position))
                            collect (apply #'make-instance 'color-button
                                           :item item
                                           (append help-key-args callback-args color-args title-args
                                                   title-position-args button-args)))))
         (layout-args (or layout-args
                          (when (subtypep layout-class 'column-layout)
                            '(:adjust :right))))
         (layout (apply #'make-instance layout-class
                        :description buttons
                        layout-args)))
    (apply #'call-next-method self
           :description (list layout)
           :buttons buttons
           (sans initargs
                 :buttons :items :callback :layout-class :layout-args :help-keys
                 :color-function :title-function :button-args :button-args-function))))
    
