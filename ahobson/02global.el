;; ensure buffers are named uniquely
(require 'grep)
(require 'uniquify)

;; make it easier to type
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(setq confirm-kill-emacs 'y-or-n-p)
(setq create-lockfiles nil)

;; options for when emacs is not in a terminal
(when window-system
  (when  (eq system-type 'darwin)
    ;; (set-face-attribute 'default nil :family "Source Code Pro" :height 110
    ;;                     :weight 'normal :width 'normal)
    (if (version< "27.0" emacs-version)
        (set-fontset-font "fontset-default" 'unicode
                          "Apple Color Emoji" nil 'prepend))
    (cond ((x-list-fonts "SF Mono")
           (set-face-attribute 'default nil :font "SF Mono" :height 120))

          ((x-list-fonts "Inconsolata")
           (set-face-attribute 'default nil :font "Inconsolata" :height 130))

          (t
           (set-face-attribute 'default nil :font "PT Mono" :height 120))))
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
(setq sh-basic-offset 4)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(setq inferior-octave-startup-args '("--no-init-file"))

(defun my-osx-version ()
  (let* ((sw-ver (shell-command-to-string "sw_vers -productVersion"))
         (m (string-match "[0-9]+\.[0-9]+" sw-ver))
         (majmin (substring sw-ver m (match-end 0))))
    (concat "v" majmin)))

(defun my-linux-version ()
  (let ((debian-version (with-temp-buffer
                          (insert-file-contents "/etc/debian_version")
                          (buffer-string))))
    (cond ((string-match "^7\." debian-version)
           "wheezy"))))

(setq my-os-version
      (pcase system-type
        (darwin (my-osx-version))
        (gnu/linux (my-linux-version))))

;;; 02global.el ends here
