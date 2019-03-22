(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package + straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; useful navigation packages
(use-package ido-completing-read+
  :config
  ;; ido-mode is like magic pixie dust!
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10))

(use-package flx-ido
  :config
  (flx-ido-mode t))

(use-package smex
  :config
  (setq smex-save-file
        (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package company)

;; lsp
(use-package lsp-mode
  :commands lsp
  :hook (programming-mode . lsp))
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)

;; clojure editing
(use-package cider)
(use-package paredit)
(use-package clojure-mode)

;; scala
(use-package ensime
  :config
  (setq ensime-default-java-flags
        (list
         "-Xss2m" "-Xms4G" "-Xmx4G"
         "-XX:ReservedCodeCacheSize=256m"
         "-XX:MaxMetaspaceSize=512m")))

;; python
(use-package jedi)

;; ruby
(use-package enh-ruby-mode :disabled)
(use-package lsp-ruby :disabled)

(use-package idle-highlight-mode)

;; in case puppet changes are needed
;;(use-package auto-pair)

(use-package puppet-mode)

;; terraform
(use-package terraform-mode)

;; Useful for git
(use-package ibuffer-vc)
(use-package git-commit)

;; Use ack for searching
(use-package ack-mode
  :straight (ack-mode
             :type git :host github
             :repo "sudish/ack-mode.el")
  :no-require t
  :config
  (load "ack-mode.el")
  :custom
  (ack-mode-program-name (or (executable-find "ag")
                             (executable-find "ack-grep")
                             (executable-find "ack"))))

(use-package project-anchor
  :straight (project-anchor
             :type git :host github
             :repo "ahobson/project-anchor"))


;; sometimes grep is what we want
(use-package grep-in-project
  :straight (grep-in-project
             :type git :host github
             :repo "ahobson/grep-in-project"))

(use-package find-file-in-project
  :straight (find-file-in-project
             :type git :host github
             :repo "ahobson/find-file-in-project"))

;; git
(use-package magit-popup)
(use-package magit
  :config (global-git-commit-mode))

;; golang
(use-package go-mode)
(use-package lsp-go
  :disabled
  :hook (go-mode . lsp))

;; handy development modes
(use-package dockerfile-mode)
(use-package smartparens)
(use-package yaml-mode)
(use-package scss-mode)
(use-package robe-mode
  :disabled)
(use-package rainbow-mode)
(use-package yasnippet)
(use-package pyvenv)
(use-package lua-mode)
(use-package vue-mode)
(use-package redenv
  :straight (redenv
             :type git :host github
             :repo "ahobson/redenv.el"))
;; editing server
(use-package edit-server)
(use-package markdown-mode)

;; visual themes
(use-package mac-classic-theme
  :disabled
  :straight (mac-classic-theme
             :type git :host github
             :repo "ahobson/mac-classic-theme")
  :config
  (load-theme 'mac-classic))

(use-package windmove
  :straight (windmove
             :type git :host github
             :repo "ahobson/windmove.el")
  :bind (([C-s-left] . (lambda () (interactive) (windmove-left -1)))
         ([C-s-up] . (lambda () (interactive) (windmove-up -1)))
         ([C-s-right] . (lambda () (interactive) (windmove-right -1)))
         ([C-s-down] . (lambda () (interactive) (windmove-down -1)))))

;; mostly stolen from the old emacs starter kit
(setq my-system-config (concat user-emacs-directory system-name ".el")
      my-user-config (concat user-emacs-directory user-login-name ".el")
      my-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path my-user-dir)

(when (file-exists-p my-system-config) (load my-system-config))

(when (file-exists-p my-user-config) (load my-user-config))

(when (file-exists-p my-user-dir)
  (mapc 'load (directory-files my-user-dir nil "^[^#].*el$")))

