;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/fm-plugin-tools/packages.lisp,v 1.16 2010/07/22 09:38:06 edi Exp $

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

(in-package :cl-user)

(defpackage :fm-plugin-tools
  (:nicknames :fm)
  (:use :cl :fli :dspec)
  (:add-use-defaults t)
  (:export :*company-name*
           :*copyright-message*
           :*enable-idle-messages*
           :*fm-logfile*
           :*gc-interval*
           :*init-function*
           :*log-backtraces-p*
           :*log-errors-p*
           #+:macosx
           :*plugin-bundle-identifier*
           :*plugin-help-text*
           :*plugin-id*
           :*plugin-name*
           :*plugin-version*
           :*preferences-function*
           :*product-name*
           :*results*
           :*shutdown-function*
           :*symbols-to-keep*
           :add-data
           :add-fnam-data
           :add-size-data
           :alpha
           :any-face-enabled-p
           :append-text
           :as-binary-data
           :as-boolean
           :as-date
           :as-day-month-year
           :as-fix-pt-object
           :as-float
           :as-integer
           :as-second-minute-hour
           :as-seconds-since-epoch
           :as-seconds-since-midnight
           :as-string
           :as-text-object
           :as-time
           :as-timestamp
           :as-universal-time
           :binary-data-object
           :blue
           :boolean-value
           :check-plugin-id
           :color
           :color-enabled-p
           :color-object
           :data-object
           :date-time-object
           :define-plugin-function
           :delete-text
           :disable-all
           :disable-all-faces
           :evaluate
           :execute-sql
           :face
           :face-enabled-p
           :fix-pt-object
           :fm-log
           :font
           :font-enabled-p
           :get-count
           :get-data
           :get-day
           :get-default-style
           :get-fnam-data
           :get-font-id
           :get-font-info
           :get-hour
           :get-index
           :get-minute
           :get-month
           :get-second
           :get-size
           :get-size-data
           :get-style
           :get-total-size
           :get-type
           :get-year
           :green
           :handle-idle-message
           :insert-text
           :kdeflt-fixed-precision
           :locale-object
           :make-binary-data-object
           :make-color-object
           :make-data-object
           :make-date-time-object
           :make-fix-pt-object
           :make-locale-object
           :make-style-object
           :make-text-object
           :nth-arg
           :plugin-preference
           :precision
           :red
           :remember-interface-geometry
           :remove-all
           :remove-data
           :remove-style
           :reset-all-style-buffers
           :set-product-name
           :set-style
           :set-text
           :set-value
           :size
           :size-enabled-p
           :start-script
           :style-object
           :text-object
           :top-level-hook
           :update-global-environment
           :version-string))
