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

(use-package company-emoji)

(use-package company
  :config
  (add-to-list 'company-backends 'company-emoji)
  (setq company-tooltip-align-annotations t))


(use-package flycheck
  :config
  :init (global-flycheck-mode))
(flymake-mode -1)

(use-package graphql-mode)

;; typescript-language-server checks to see if the client process id
;; is still alive, and that doesn't work inside a docker container
(defun my-emacs-pid (orig-emacs-pid &rest args)
  (if lsp--cur-workspace
      1
    (apply orig-emacs-pid args)))

;; lsp now tries to find the tsserver, which we have in docker
(defun my-tsserver-path (path)
  (let ((default-directory (projectile-project-root)))
    (file-truename path)))

;; lsp
(use-package lsp-mode
  :hook ((typescript-mode . lsp-deferred)
         (rjsx-mode . lsp-deferred)
         (python-node . lsp-deferred)
         (go-mode . lsp-deferred))
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 1600000)
  (advice-add 'emacs-pid :around #'my-emacs-pid)
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.pre-commit-cache$")
  (eval-after-load 'lsp-clients
    '(progn
       (plist-put lsp-deps-providers :docker (list :path #'my-tsserver-path))
       (lsp-dependency 'typescript `(:docker "./node_modules/typescript/bin/tsserver"))))
  :commands lsp lsp-deferred)
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
  :straight (ensime-emacs :type git
                          :host github
                          :repo "ensime/ensime-emacs"
                          :branch "2.0")
  :config
  (setq ensime-default-java-flags
        (list
         "-Xss2m" "-Xms4G" "-Xmx4G"
         "-XX:ReservedCodeCacheSize=256m"
         "-XX:MaxMetaspaceSize=512m"))
  :after (scala-mode))

;; json
(use-package json-mode)

;; typescript

(use-package typescript-mode
  :after (company flycheck))

(use-package prettier-js
  :after (typescript-mode rjsx-mode)
  :hook ((typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

;; react
(use-package rjsx-mode
  :after (typescript-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode)))

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

;; terraform
(use-package terraform-mode)

;; Useful for git
(use-package ibuffer-vc)
(use-package git-commit)

(use-package ag
  :custom
  (ag-executable (or (executable-find "ag")
                             (executable-find "ack-grep")
                             (executable-find "ack"))))

(use-package projectile
  :config
  (projectile-mode +1))

;; git
(use-package magit-popup)
(use-package magit
  :config (global-git-commit-mode))

;; golang
(use-package go-mode)

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

