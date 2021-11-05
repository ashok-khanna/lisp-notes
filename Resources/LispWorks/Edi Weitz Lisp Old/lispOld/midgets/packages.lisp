;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/midgets/packages.lisp,v 1.17 2013/08/23 08:04:12 edi Exp $

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

(in-package :cl-user)

(defpackage :midgets
  (:use :capi :gp)
  (:add-use-defaults t)
  (:export :*default-first-day-of-week*
           :*default-month-names*
           :*default-weekday-names*
           :*inactive-caption-color*
           :*inactive-caption-text-color*
           :*highlight-color*
           :*highlight-text-color*
           :*window-color*
           :*window-text-color*
           #+:win32 :*use-win32-color-info*
           #+:win32 :*use-win32-locale-info*
           :cache-interface-geometry
           :clear-interface-geometry-cache
           :collecting-display-pane
           :collecting-display-pane-auto-scroll-p
           :collecting-display-pane-lock
           :collecting-display-pane-stream
           :color-button
           :color-button-callback
           :color-button-color
           :color-button-panel
           :color-button-panel-buttons
           :color-button-panel-item-color
           :color-button-panel-items
           :color-button-panel-test-function
           :color-button-prompt-args
           :color-button-prompt-text
           :date-interface
           :date-interface-callback
           :date-interface-day
           :date-interface-month
           :date-interface-time
           :date-interface-year
           :display-pane-stream
           :display-pane-stream-force-output-p
           :display-pane-stream-lock
           :display-pane-stream-pane
           :flat-button
           :flat-button-callback
           :flat-button-callback-type
           :flat-button-item
           :flat-button-panel
           :flat-button-panel-buttons
           :flat-button-panel-items
           :flat-button-panel-test-function
           :get-interface-geometry
           :prompt-for-date
           :prompt-for-time
           :prompt-for-date-and-time
           :time-interface
           :time-interface-callback
           :time-interface-hour
           :time-interface-minute
           :time-interface-second
           :time-interface-time))
