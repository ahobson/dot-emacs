

(require 'package)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(setq el-get-git-install-url
      "https://github.com/dimitri/el-get.git")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; set local recipes
(setq el-get-sources
      '(
        (:name company-mode)
        (:name smartparens)
        (:name autopair)
        (:name ido-ubiquitous)
        (:name smex)
        (:name idle-highlight-mode)
        (:name git-modes)
        (:name magit
               ;; the lastest magit needs some recipe modifications
               :depends (git-modes)
               :autoloads nil
               :build (if (version<= "24.3" emacs-version)
                          `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
                        `(("make" ,(format "EMACS=%s" el-get-emacs) "docs")))
               :build/berkeley-unix (("touch" "`find . -name Makefile`") ("gmake")))
        (:name paredit)
        (:name rhtml-mode)
        (:name yaml-mode)
        (:name scss-mode)
        (:name puppet-mode)
        (:name rinari)
        (:name robe-mode)
        (:name rainbow-mode :type elpa)
        (:name octave)
        (:name clojure-mode)
        (:name cider)
        (:name edit-server)
        (:name ibuffer-vc)
        (:name yasnippet)
        (:name ack-mode)
        (:name grep-in-project)
        (:name ahobson-find-file-in-project)
        (:name ahobson-ruby-test-mode)
        (:name project-anchor)
        (:name markdown-mode)
        (:name mac-classic-theme)))

(setq my:el-get-packages
      '(el-get
        rvm))

;; example of conditional packages
(when (el-get-executable-find "svn")
  (loop for p in '(psvn    ; M-x svn-status
                   )
        do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; I'm seeing something weird in Emacs 24 where el-get pauses fetching
;; files until I hit a key.  I'm not sure what is going on.
(el-get 'sync my:el-get-packages)

;; mostly stolen from the old emacs starter kit
(setq my-system-config (concat user-emacs-directory system-name ".el")
      my-user-config (concat user-emacs-directory user-login-name ".el")
      my-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path my-user-dir)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)

(when (file-exists-p my-system-config) (load my-system-config))

(when (file-exists-p my-user-config) (load my-user-config))

(when (file-exists-p my-user-dir)
  (mapc 'load (directory-files my-user-dir nil "^[^#].*el$")))

