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
        (:name bookmark+)
        (:name rhtml-mode)
        (:name yaml-mode)
        (:name scss-mode)
        (:name edit-server
               :after (lambda ()
                        (require 'edit-server)
                        (edit-server-start)))
        ;; not ready for prime time yet
        ;; (:name ruby-electric :type elpa)
        ;; (:name emacs-rails :type git :url "git://github.com/remvee/emacs-rails.git"
        ;;        :load ("rails.el"))
        (:name ibuffer-vc :type git :url "git://github.com/purcell/ibuffer-vc.git")
        (:name yasnippet :type git :url "git://github.com/capitaomorte/yasnippet.git"
               :after (lambda ()
                        (require 'yasnippet)
                        (yas/initialize)
                        (setq yas/snippet-dirs '("~/.emacs.d/el-get/yasnippet/snippets"
                                                 "~/.emacs.d/el-get/yasnippet/extras/imported"))
                        (yas/global-mode 1)))
        (:name ack-mode :type git :url "git://github.com/sudish/ack-mode.el.git"
               :load ("ack-mode.el"))
        (:name grep-in-project :type git :url "git://github.com/ahobson/grep-in-project.git"
               :features grep-in-project)
        (:name ruby-test-mode :type git :url "git://github.com/ahobson/ruby-test-mode.git"
               :features ruby-test-mode)
        (:name project-anchor :type git :url "git://github.com/ahobson/project-anchor.git"
               :load ("project-anchor.el"))))

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
