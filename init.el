(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'package)
(add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-refresh-contents)
;; (setq package-archives
;;       '(("melpa-stable" . "http://stable.melpa.org/packages/")
;;         ("gnu" . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "http://melpa.org/packages/")
;;         ("marmalade" . "https://marmalade-repo.org/packages/")))
;; (setq package-archive-priorities '(("melpa-stable" . 10)
;;                                    ("gnu" . 5)
;;                                    ("marmalade" . 4)
;;                                    ("melpa" . 0)))
(package-initialize)
(package-refresh-contents)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


;; (unless (require 'el-get nil 'noerror)
;;   (require 'package)
;;   (setq package-archives
;; 	'(("melpa-stable" . "http://stable.melpa.org/packages/")
;; 	  ("gnu" . "http://elpa.gnu.org/packages/")
;; 	  ("melpa" . "http://melpa.org/packages/")
;; 	  ("marmalade" . "https://marmalade-repo.org/packages/")))
;;   (setq package-archive-priorities '(("melpa-stable" . 10)
;; 				     ("gnu" . 5)
;; 				     ("marmalade" . 4)
;; 				     ("melpa" . 0)))
;;   (package-refresh-contents)
;;   (package-initialize)
;;   (package-install 'el-get)
;;   (require 'el-get))

(setq el-get-user-package-directory "~/.emacs.d/el-get-user-package")

;; clojure editing
(package-install 'cider)
(package-install 'paredit)
(package-install 'clojure-mode)
(package-install 'company)
(company-mode)

;; scala
;; ;(el-get-bundle elpa:scala-mode2)
;; (el-get-bundle elpa:ensime)
(package-install 'ensime)
(package-install 'scala-mode2)
(setq ensime-default-java-flags
      (list
       "-Xss2m" "-Xms4G" "-Xmx4G"
       "-XX:ReservedCodeCacheSize=256m"
       "-XX:MaxMetaspaceSize=512m"))

;; python
(package-install 'jedi)

;; ruby
(package-install 'enh-ruby-mode)

;; useful navigation packages
(package-install 'ido-completing-read+)
(package-install 'flx-ido)
(package-install 'ido-ubiquitous)
(flx-ido-mode t)
;; ido-mode is like magic pixie dust!
(ido-mode t)
;(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(package-install 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(package-install 'idle-highlight-mode)

;; in case puppet changes are needed
(package-install 'autopair)
(package-install 'puppet-mode)

;; ;; Useful for git
(package-install 'ibuffer-vc)
(package-install 'git-commit)

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
(package-install 'magit-popup)
(package-install 'magit)
(global-git-commit-mode)

;; handy development modes
(package-install 'dockerfile-mode)
(package-install 'smartparens)
(package-install 'yaml-mode)
(package-install 'scss-mode)
(el-get-bundle robe-mode)
(package-install 'rainbow-mode)
(package-install 'yasnippet)
(package-install 'go-mode)
(package-install 'pyvenv)
(package-install 'lua-mode)
(package-install 'vue-mode)
(el-get-bundle redenv
  :url "https://github.com/ahobson/redenv.el.git"
  :features redenv)

;; editing server
(package-install 'edit-server)
(package-install 'markdown-mode)

;; visual themes
(el-get-bundle mac-classic-theme)

(el-get-bundle windmove
  :url "https://github.com/ahobson/windmove.el.git"
  :features windmove)

;; mostly stolen from the old emacs starter kit
(setq my-system-config (concat user-emacs-directory system-name ".el")
      my-user-config (concat user-emacs-directory user-login-name ".el")
      my-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path my-user-dir)

(when (file-exists-p my-system-config) (load my-system-config))

(when (file-exists-p my-user-config) (load my-user-config))

(when (file-exists-p my-user-dir)
  (mapc 'load (directory-files my-user-dir nil "^[^#].*el$")))

