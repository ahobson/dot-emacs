;; ensure buffers are named uniquely
(require 'grep)
(require 'uniquify)

;; make it easier to type
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; options for when emacs is not in a terminal
(when window-system
  (add-to-list 'initial-frame-alist '(height . 60))
  (add-to-list 'initial-frame-alist '(width . 162))
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (tool-bar-mode -1))

(setq custom-file "~/.emacs.d/custom.el")

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

;; show matching parens
(show-paren-mode 1)

;; set up ack mode to use the project-anchor project to find the root
;; of the project
(setq ack-root-directory-function 'project-anchor-find-from-default-directory)
(setq ack-mode-root-directory-function 'project-anchor-find-from-default-directory)

;; let the project anchor be defined by file or by dired mark
(add-hook 'project-anchor-find-hook 'project-anchor-find-by-file)
(add-hook 'project-anchor-find-hook 'project-anchor-find-with-mark)

;; let the project anchor be defined by file or by dired mark
(setq ffip-project-root-function 'project-anchor-find-from-default-directory)
(setq ack-mode-root-directory-function 'project-anchor-find-from-default-directory)

;; auto indent by pressing tab
(set-default 'indent-tabs-mode nil)
;; point out empty lines
(set-default 'indicate-empty-lines t)

;; in text mode, add some help
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Show whitespace problems
(add-hook 'prog-mode-hook 'my-turn-on-whitespace)
(add-hook 'puppet-mode-hook 'my-turn-on-whitespace)

;; Use paredit for emacs lisp
(add-hook 'emacs-lisp-mode-hook 'my-turn-on-paredit)

(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

;; java/javascript
(setq js-indent-level 2)
(add-hook 'java-mode-hook 'my-c-basic-offset)

;; shell
(setq sh-basic-offset 2)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(defun my-osx-version ()
  (let ((sw-ver (shell-command-to-string "sw_vers -productVersion")))
    (cond ((string-match "^10\.9" sw-ver)
           "mavericks")

          ((string-match "^10\.10" sw-ver)
           "yosemite"))))

(defun my-linux-version ()
  (let ((debian-version (with-temp-buffer
                          (insert-file-contents "/etc/debian_version")
                          (buffer-string))))
    (cond ((string-match "^7\." debian-version)
           "wheezy"))))

(setq my-os-version
      (case system-type
        ('darwin (my-osx-version))
        ('gnu/linux (my-linux-version))))
