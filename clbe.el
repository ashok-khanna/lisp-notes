;; https://nullprogram.com/blog/2013/02/06/

(define-minor-mode clbe-mode
  "Common Lisp By Example - Lisp Guides"
  :lighter " clbe"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-b C-e") 'show-documentation)
	    (define-key map [down-mouse-3] (kbd "<mouse-2>"))
	    map))

(require 'button)
(defvar guest63-button-map2
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m 'button-map)
    (define-key m (kbd "<mouse-2>")) m))


(defun show-documentation ()
  (interactive)
  (let ((word (thing-at-point 'symbol))
        (temp-buf-name "*stuffed*"))
    ;; Create a buffer `temp-buf-name' and run the shell command in it
    (get-buffer-create temp-buf-name)
    ;; Switch to the `temp-buf-name' buffer
    (pop-to-buffer-same-window temp-buf-name)
    (insert word)
    (insert "\n")
    (insert "Go to documentation: ")
    (insert-text-button "Google" 'action (lambda (x) (eww "https://www.google.com")))
    
    ;; Set its major mode to `special-mode'
    (help-mode) ; a read-only major mode
    ;; As you are now in the `temp-buf-name' buffer, you can do the below:
    ;; - hit `q' to quit the current window *without* killing `temp-buf-name'
    ;; - hit `C-u q' to quit the current window *and* kill `temp-buf-name'
    ))

(defun show-documentation ()
  (interactive)
  (let ((word (thing-at-point 'symbol)))
    (with-output-to-temp-buffer "my-buffer"
      (princ word)
      "URL `https://www.google.com'")))
;; (with-help-window "*My Help*" (princ "There's a message here.")
  ;; 		    (princ "URL https://www.google.com"))

(defun show-documentation ()
  "On key command C-a C-b bring up new buffer in the same window. Links should be clickable via ENTER and via mouse click."
  (interactive)
  (let ((word (thing-at-point 'symbol)))
    (pop-to-buffer-same-window "My Docs")
    (erase-buffer)
    (insert "You have searched for: ")
    (insert word)
    (insert "\n")
    (insert "Go to documentation: ")
    (insert-text-button "Google" 'action (lambda (x) (eww "https://www.google.com")))
    (view-mode t)))

(defun show-documentation ()
  (interactive)
  (let ((word (thing-at-point 'symbol)))
    (switch-to-buffer "CLBE")
    (erase-buffer)
    (insert word)
    (insert-text-button "Math.Dev" 'action (lambda(x) (eww "https://www.math.dev"))
			'mouse-action (lambda (x) (eww "https://www.google.com")))
    (insert "The home page for the GNU project has more information (see URL
`https://www.gnu.org/').")))

(add-hook 'lisp-mode-hook 'clbe-mode)

(provide 'clbe-mode)


 (message word)
    (switch-to-buffer "*Messages*")

(with-output-to-temp-buffer)
