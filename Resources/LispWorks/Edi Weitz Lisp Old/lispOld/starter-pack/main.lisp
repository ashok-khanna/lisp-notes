;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STARTER-PACK; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/starter-pack/main.lisp,v 1.14 2011/02/11 19:56:05 edi Exp $

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

(defun check-version ()
  (let* ((changelog-url "http://weitz.de/starter-pack/CHANGELOG.txt")
         (current-version
          (ignore-errors 
            (ppcre:scan-to-strings "(?<=Version )[\\d.]+"
                                   (drakma:http-request changelog-url
                                                        :proxy (get-proxy changelog-url)
                                                        :user-agent (user-agent-string))))))
    (unless current-version
      (capi:display-message "Cannot determine most recent version of Lisp Starter Pack.
You'll have to check <http://weitz.de/starter-pack/> manually.")
      (return-from check-version))
    (when (version< (starter-pack-version-string) current-version)
      (capi:display-message "It looks like you're not using the most recent version of this program.
You have ~A, but ~A is available from <http://weitz.de/starter-pack/>.
You should upgrade before proceeding."
                            (starter-pack-version-string) current-version))))

(defun do-the-work (interface)
  "The function which does all the work of downloading and
installing the selected libraries and the supporting files."
  (handler-case
      (progn
        (set-status "Cleaning target directory.")
        (fad:delete-directory-and-files (lib-dir) :if-does-not-exist :ignore)
        (set-status "Downloading ASDF.")
        (get-asdf)
        #+:win32
        (when (clsql-selected-p)
          (set-status "Downloading SQLite DLL.")
          (get-sqlite))
        (get-libs #'set-status)
        (set-status "Downloading file \"start.lisp\".")
        (get-start-file))
    (error (cond)
      (capi:display-message "~A" cond)
      (quit-interface interface))))

(defun maybe-do-the-work (ok-button interface)
  "Ask the user if he really wants to invoke DO-THE-WORK and then
invokes it \(in a separate thread) if this is confirmed."
  (when (capi:confirm-yes-or-no "Delete contents of ~A
and download and install ~A libraries there?"
                                (lib-dir) (count-if (lambda (lib)
                                                      (or (checked lib)
                                                          (required lib))) *libs*))
    ;; disable all buttons
    (dolist (button (append (layout-description (element-parent ok-button))
                            *lib-buttons*))
      (when (typep button 'button)
        (setf (button-enabled button) nil)))
    (let (done)
      ;; start a new thread for DO-THE-WORK
      (mp:process-run-function "Downloading libraries." nil
                               (lambda ()
                                 (do-the-work interface)
                                 (setq done t)))
      ;; wait for DONE to become true, but make sure the interface is
      ;; still responsive
      (mp:wait-processing-events most-positive-fixnum
                                 :wait-reason "Waiting for libraries to be downloaded."
                                 :wait-function (lambda () done)))
    (capi:display-message "That's it.  Good-bye...")
    (quit-interface interface)))

(defun main ()
  "The main function called by the delivered executable."
  (set-proxy-info)
  (read-config-file)
  (read-my-libs)
  (compute-dependencies)
  (check-version)
  (display (create-interface)))
