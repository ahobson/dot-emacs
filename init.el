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

;;
;; devbox global add cmake libtool mktemp
;; ln -s =libtool bin/glibtool
;; sudo ln -s =mktemp /usr/local/bin
(use-package vterm)

(use-package eat
  :straight (eat
             :type git :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el"))))


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

;; try out envrc mode instead of direnv
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package flycheck
  :config
  :init (global-flycheck-mode))
(flymake-mode -1)

(use-package graphql-mode)

;; kotlin for gradle build files
(use-package kotlin-mode)

;;(use-package lsp-mode)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package lsp-java
  :hook (java-mode . (lambda ()
                       (lsp-deferred)))
  :config
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.40.0/jdt-language-server-1.40.0-202409261450.tar.gz")
  (setq lsp-java-server-install-dir "/Users/hobsoand/src/jdtls/")
  (setq lsp-java-workspace-dir "/Users/hobsoand/.emacs.d/workspace/"))

(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

(use-package lsp-treemacs)

;; lsp
(use-package lsp-mode
  :hook ((typescript-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (ruby-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (sql-mode .lsp-deferred)
         (sql-interactive-mode . lsp-deferred))
  :config
  (setq lsp-semgrep-metrics-enabled nil)
  (setq lsp-disabled-clients '(semgrep-ls ruby-ls rubocop-ls))
  (setq lsp-prefer-flymake nil)
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 1600000)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pre-commit-cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.gopath\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.npmglobal\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.log\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.drew\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]coverage\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]playwright-report\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]playwright/html-report\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]playwright/results\\'")
  (setq lsp-completion-provider :capf)

  ;; trying out sql
  (setq sql-backend 'lsp)
  (setq sql-lsp-sqls-workspace-config-path 'workspace)
  (setq lsp-sqls-connections
    '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres dbname=dev_db sslmode=disable"))))

  :commands lsp lsp-deferred)
(use-package lsp-ui
  :commands lsp-ui-mode)

;; clojure editing
(use-package cider)
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
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

(use-package jest-test-mode
  :after (typescript-mode))

;; python
;;(use-package jedi)
;; not working in emacs 29 nightly
;;(use-package python-pytest)
(use-package python-black)
;; don't want all of elpy, so maybe?
(use-package elpy
  :hook (python-mode . (lambda () (local-set-key (kbd "C-c C-;") 'elpy-test))))

;; ruby

(use-package inf-ruby)
(use-package rspec-mode)
(use-package ruby-test-mode)
;;(use-package ruby-ts-mode)
;; (use-package robe
;;   :config
;;   (add-hook 'ruby-mode-hook #'robe-mode)
;;   (add-hook 'ruby-ts-mode-hook #'robe-mode))
(use-package projectile-rails
  :after (projectile)
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package idle-highlight-mode)

;; in case puppet changes are needed
;;(use-package auto-pair)

(use-package puppet-mode)

(use-package nix-mode)

;; terraform
(use-package terraform-mode
  :custom (terraform-format-on-save t)
  :hook (terraform-mode . (lambda ()
                          (outline-minor-mode))))

;; Useful for git
(use-package ibuffer-vc)
;; this is part of magit
;;(use-package git-commit)

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

;; not working in emacs 30, not sure why
;;(use-package scss-mode)
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
;; editing server
(use-package edit-server)
(use-package markdown-mode)

;; github codespaces
(use-package codespaces
  :config (codespaces-setup))

;; needed to find ruby in codespaces
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-completions '((t . (extrabold))))
  ;; Load the theme of your choice:
  (load-theme 'modus-operandi t))

(use-package windmove
  :straight (windmove
             :type git :host github
             :repo "ahobson/windmove.el")
  :bind (([C-s-left] . (lambda () (interactive) (windmove-left -1)))
         ([C-s-up] . (lambda () (interactive) (windmove-up -1)))
         ([C-s-right] . (lambda () (interactive) (windmove-right -1)))
         ([C-s-down] . (lambda () (interactive) (windmove-down -1)))))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el" "dall-e-shell.el")))

;; security add-generic-password -a $LOGNAME -s openapi-key -w "thekey"
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-info-password
         (nth 0
              (auth-source-macos-keychain-search
               :backend (auth-source-backend-parse 'macos-keychain-generic)
               :user "ahobson" :port 'openapi-key
               :type 'macos-keychain-generic :max 1)))))

(setq dall-e-shell-openai-key chatgpt-shell-openai-key)

;; treesitter exploration
(use-package tree-sitter)
(use-package tree-sitter-langs)
;; (setq treesit-language-source-alist
;;       '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

;; mostly stolen from the old emacs starter kit
(setq my-system-config (concat user-emacs-directory (system-name) ".el")
      my-user-config (concat user-emacs-directory user-login-name ".el")
      my-user-dir (concat user-emacs-directory user-login-name))

(when (file-exists-p my-system-config) (load my-system-config))

(add-to-list 'load-path my-user-dir)

(when (file-exists-p my-user-config)
  (load my-user-config))

(when (file-exists-p my-user-dir)
  (mapc 'load (directory-files my-user-dir nil "^[^#].*el$")))

;;; init.el ends here
