(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'package)
(add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-user-package-directory "~/.emacs.d/el-get-user-package")

;; clojure editing
(el-get-bundle elpa:paredit)
(el-get-bundle elpa:clojure-mode)
(el-get-bundle elpa:cider)
(el-get-bundle elpa:company)

;; useful navigation packages
(el-get-bundle elpa:ido-ubiquitous)
(el-get-bundle elpa:smex)
(el-get-bundle elpa:idle-highlight-mode)

;; in case puppet changes are needed
(el-get-bundle elpa:autopair)
(el-get-bundle elpa:puppet-mode)

;; Useful for git
(el-get-bundle elpa:ibuffer-vc)
(el-get-bundle elpa:git-commit-mode)

;; Use ack for searching
(el-get-bundle ack-mode
  :url "https://github.com/sudish/ack-mode.el.git"
  (load "ack-mode.el"))
(el-get-bundle project-anchor
  :url "https://github.com/ahobson/project-anchor.git"
  :features project-anchor)

;; sometimes grep is what we want
(el-get-bundle grep-in-project
  :url "https://github.com/ahobson/grep-in-project.git"
  :features grep-in-project)

(el-get-bundle find-file-in-project
  :url "https://github.com/ahobson/find-file-in-project.git"
  :features find-file-in-project)

;; git
(el-get-bundle elpa:magit)

;; handy development modes
(el-get-bundle elpa:smartparens)
(el-get-bundle elpa:yaml-mode)
(el-get-bundle elpa:scss-mode)
(el-get-bundle robe-mode)
(el-get-bundle rainbow-mode)
(el-get-bundle elpa:yasnippet)

;; editing server
(el-get-bundle elpa:edit-server)
(el-get-bundle elpa:markdown-mode)

;; visual themes
(el-get-bundle mac-classic-theme)

;; mostly stolen from the old emacs starter kit
(setq my-system-config (concat user-emacs-directory system-name ".el")
      my-user-config (concat user-emacs-directory user-login-name ".el")
      my-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path my-user-dir)

(when (file-exists-p my-system-config) (load my-system-config))

(when (file-exists-p my-user-config) (load my-user-config))

(when (file-exists-p my-user-dir)
  (mapc 'load (directory-files my-user-dir nil "^[^#].*el$")))

