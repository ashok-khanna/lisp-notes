;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIDGETS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/geometry.lisp,v 1.3 2013/08/23 08:04:12 edi Exp $

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

(defun clear-interface-geometry-cache ()
  "Clears the internal geometry cache.  This function should for
example be called before an image is delivered."
  (clrhash *interface-geometry-cache*)
  (values))

(defmethod get-interface-geometry ((interface interface) &optional screen)
  "Returns the cached or saved geometry for the CAPI interface
INTERFACE and the screen SCREEN.  It first tries to find it in the
internal cache of the MIDGETS library and, failing that, tries to look
it up using LW:USER-PREFERENCE.

The returned geometry is a list of four elements - the absolute x and y
coordinates of the interface and its width and height.

This will only work for interfaces which are declared as interfaces
that should be saved in the usual \(CAPI) way."
  (when (top-level-interface-save-geometry-p interface)
    (multiple-value-bind (key product)
        (top-level-interface-geometry-key interface)
      (or (gethash key *interface-geometry-cache*)
          (first
           (let ((screen (or screen
                             (convert-to-screen interface)
                             (convert-to-screen))))
             ;; this is partly undocumented but unlikely to change in the future...
             (lw:user-preference `("Environment" ,(package-name (symbol-package key))
                                                 ,(symbol-name key))
                                 (format nil "Window Positions:~Ax~A"
                                         (screen-width screen)
                                         (screen-height screen))
                                 :product product)))))))

(defmethod cache-interface-geometry ((interface interface))
  "Saves the geometry for the CAPI interface INTERFACE in the internal
cache of the MIDGETS library.

This will only work for interfaces which are declared as interfaces
that should be saved in the usual \(CAPI) way."
  (when (top-level-interface-save-geometry-p interface)
    (let ((key (top-level-interface-geometry-key interface)))
      (setf (gethash key *interface-geometry-cache*)
            (multiple-value-list
             (top-level-interface-geometry interface))))))

