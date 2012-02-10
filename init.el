;;
;;
;; modified/stolen from
;; https://github.com/dimitri/emacs-kicker/blob/master/init.el
;;
(require 'cl)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  ;; I like it synchronous so that I know that the rest of the file will
  ;; execute after el-get is installed
  (progn
    (switch-to-buffer
     (url-retrieve-synchronously
      "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))
     (end-of-buffer)
     (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set up elpa with the marmalade repo ... but use el-get to install
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; set local recipes
(setq el-get-sources
      '((:name starter-kit :type elpa)
        (:name starter-kit-ruby :type elpa)
        (:name bookmark+ :type elpa)
        (:name yasnippet :type git :url "git://github.com/capitaomorte/yasnippet.git")
        (:name ack-mode :type git :url "git://github.com/sudish/ack-mode.el.git"
               :load ("ack-mode.el"))
        (:name grep-in-project :type git :url "git://github.com/ahobson/grep-in-project.git"
               :features grep-in-project)
        (:name ruby-test-mode :type git :url "git://github.com/r0man/ruby-test-mode.git"
               :features ruby-test-mode)))

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
