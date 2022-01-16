Below are some brief notes on customising LispWorks.


The main LispWorks user configuration settings are saved in a file called `.lispworks` (well I guess this could be renamed to any file).
You can set the path to this configuration file by going (on Mac) LispWorks > Preferences and then under Environment / General,
at the bottom, there is an option to select the initialization file.

Below is what my .lispworks looked at the time of writing:

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

;; ASDF and QuickLisp installation

(require "asdf")

(when (probe-file "~/.quicklisp/setup.lisp")
  (load "~/.quicklisp/setup.lisp"))

(ql:quickload 'flexi-streams)

(push #p"/Users/ashokkhanna/.emacs.d/elpa/sly-20211222.1234/slynk/" ASDF:*CENTRAL-REGISTRY*)

(asdf:require-system :slynk)

;; (slynk:create-server :port 4008  :dont-close t)

;; Indent on new lines

(editor::bind-key "Indent New Line" #\Return)

;; Editor Commands

(editor::bind-key "Next Ordinary Window" "Meta-o")
(editor::bind-key "Split Window Vertically" #("Ctrl-x" "2"))
(editor::bind-key "Split Window Horizontally" #("Ctrl-x" "3"))
(editor::bind-key "New Window" #("Ctrl-x" "3"))
(editor::bind-key "Evaluation Region" "Ctrl-Meta-x")
(editor::bind-key "Compile Region" #("Ctrl-c" "Ctrl-k"))

(declaim (optimize (speed 0) (safety 3) (debug 3)))


;; (asdf:load-system "lw-paredit")

;; (bind-key "All Vim" "escape" :global :emacs)

;; (asdf:load-system "lw-vim-mode")
```

Note here that this loads SLYNK / QuickLisp / etc. so that we can remote into LispWorks via Emacs with the following in LW REPL:

```lisp
(slynk:create-server :port 4006  :dont-close t)
```

And then in Emacs do M-x sly-connect (make sure to change the port to 4006).

[Need to add note on keybindings and other configuration options]

