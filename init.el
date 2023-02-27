;;; package -- ahobson init file
;;; Commentary:
;;; Trying out config
;;; Code:
(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

(if (file-exists-p (expand-file-name "~/.nix-profile/bin"))
    (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin")))

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

(use-package vterm)

(use-package smex
  :config
  (setq smex-save-file
        (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package company-emoji)

(use-package company
  :config
  (add-to-list 'company-backends 'company-emoji)
  (setq company-tooltip-align-annotations t))

(use-package direnv
  ;; :straight (direnv
  ;;            :repo "siddharthverma314/emacs-direnv"
  ;;            :fetcher git
  ;;            :branch "master")
  :config
  (direnv-mode))

(use-package flycheck
  :config
  :init (global-flycheck-mode))
(flymake-mode -1)

(use-package graphql-mode)

;; typescript-language-server checks to see if the client process id
;; is still alive, and that doesn't work inside a docker container
(defun my-emacs-pid (orig-emacs-pid &rest args)
  "Hack for lsp typescript-language-server inside docker or returning ORIG-EMACS-PID (as ARGS)."
  (if (and lsp--cur-workspace (not (file-exists-p "nix")))
      1
    (apply orig-emacs-pid args)))

;; lsp now tries to find the tsserver, which we have in docker
(defun my-tsserver-path (path)
  "Hack for lsp tsserver inside docker using PATH."
  (let ((default-directory (projectile-project-root)))
    (file-truename path)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; lsp
(use-package lsp-mode
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (web-mode . lsp-deferred)
;         (python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (sql-mode .lsp-deferred)
         (sql-interactive-mode . lsp-deferred))
  :config
  (setq lsp-prefer-flymake nil)
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 1600000)
  (advice-add 'emacs-pid :around #'my-emacs-pid)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pre-commit-cache$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.gopath$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.npmglobal$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.log$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\coverage$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\tmp$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\playwright-report$")
  (setq lsp-completion-provider :capf)

  ;; trying out sql
  (setq sql-backend 'lsp)
  (setq sql-lsp-sqls-workspace-config-path 'workspace)
  (setq lsp-sqls-connections
    '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres dbname=dev_db sslmode=disable"))))

  ;; (eval-after-load 'lsp-clients
  ;;   '(progn
  ;;      (plist-put lsp-deps-providers :docker (list :path #'my-tsserver-path))
  ;;      (lsp-dependency 'typescript `(:docker
  ;;   "./node_modules/typescript/bin/tsserver"))))

  :commands lsp lsp-deferred)
(use-package lsp-ui
  :commands lsp-ui-mode)

;; clojure editing
(use-package cider)
(use-package paredit
  :config
  (add-hook 'fennel-mode-hook #'paredit-mode))
(use-package clojure-mode)

;; json
(use-package json-mode)

;; typescript

(use-package typescript-mode
  :after (company flycheck))

(use-package prettier-js
  :hook ((typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)))

;; react

(defun setup-web-jtsx ()
  "Setup web for tsx."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (when (or (string-equal "jsx" (file-name-extension buffer-file-name))
            (string-equal "tsx" (file-name-extension buffer-file-name)))
    (prettier-js-mode)))

(use-package web-mode
  :hook (web-mode . setup-web-jtsx)
  :mode "\\.[jt]sx\\'")

(use-package jest)

;; python
(use-package jedi)
(use-package python-pytest)
(use-package python-black)

;; ruby

(use-package inf-ruby)
(use-package rspec-mode
  :config
  (defun rspec-docker-p ()
    t))

(use-package idle-highlight-mode)

;; in case puppet changes are needed
;;(use-package auto-pair)

(use-package puppet-mode)

(use-package nix-mode)

;; terraform
(use-package terraform-mode)

;; Useful for git
(use-package ibuffer-vc)
(use-package git-commit)

(use-package deadgrep
  :bind ("C-c a" . 'deadgrep))

(use-package projectile
  :config
  (projectile-mode +1))

;; git
(use-package magit-popup)
(use-package magit
  :config (global-git-commit-mode))

;; golang

(defun setup-go-mode ()
  "Setup go mode."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-mode
  :hook (go-mode . setup-go-mode))
(use-package gotest)

;; handy development modes
(use-package dockerfile-mode)
(use-package smartparens)
(use-package yaml-mode)
(use-package scss-mode)
(use-package sass-mode)
(use-package robe-mode
  :disabled)
(use-package editorconfig
  :config
  (editorconfig-mode 1))
(use-package rainbow-mode)

(use-package restclient)
(use-package yasnippet
  :config (yas-global-mode))
(use-package pyvenv)
(use-package lua-mode)
(use-package fennel-mode)
(use-package vue-mode)
(use-package redenv
  :straight (redenv
             :type git :host github
             :repo "ahobson/redenv.el"))
;; editing server
(use-package edit-server)
(use-package markdown-mode)

;; visual themes
;; (use-package mac-classic-theme
;;   :disabled
;;   :straight (mac-classic-theme
;;              :type git :host github
;;              :repo "ahobson/mac-classic-theme")
;;   :config
;;   (load-theme 'mac-classic))

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-lang-checkers '(intense)
        modus-themes-syntax '(yellow-comments green-strings alt-syntax)
        modus-themes-paren-match '(bold)
        modus-themes-links '(neutral-underline))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi))

(use-package windmove
  :straight (windmove
             :type git :host github
             :repo "ahobson/windmove.el")
  :bind (([C-s-left] . (lambda () (interactive) (windmove-left -1)))
         ([C-s-up] . (lambda () (interactive) (windmove-up -1)))
         ([C-s-right] . (lambda () (interactive) (windmove-right -1)))
         ([C-s-down] . (lambda () (interactive) (windmove-down -1)))))

(use-package elfeed
  :config
  (setq elfeed-sort-order 'ascending))
(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org))

;; mostly stolen from the old emacs starter kit
(setq my-system-config (concat user-emacs-directory system-name ".el")
      my-user-config (concat user-emacs-directory user-login-name ".el")
      my-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path my-user-dir)

(when (file-exists-p my-system-config) (load my-system-config))

(when (file-exists-p my-user-config) (load my-user-config))

(when (file-exists-p my-user-dir)
  (mapc 'load (directory-files my-user-dir nil "^[^#].*el$")))

;;; init.el ends here
