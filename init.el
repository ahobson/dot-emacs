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
      '((:name autopair)
        (:name ido-ubiquitous)
        (:name smex)
        (:name starter-kit
               :type elpa)
        (:name starter-kit-ruby :type elpa)
        (:name rhtml-mode)
        (:name yaml-mode)
        (:name scss-mode)
        (:name puppet-mode)
        (:name rinari)
        (:name rainbow-mode :type elpa)
        (:name octave
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/progmodes/octave.el"
               :after (progn
                        (require 'octave)))
        (:name clojure-mode)
        (:name nrepl)
        (:name edit-server
               :after (progn
                        (require 'edit-server)
                        (edit-server-start)))
        ;; (:name ruby-mode-indent-fix
        ;;        :type http
        ;;        :url "https://raw.github.com/lewang/le_emacs_libs/master/ruby-mode-indent-fix.el"
        ;;        :after (lambda ()
        ;;                 (require 'ruby-mode-indent-fix)))
        (:name ibuffer-vc
               :type github
               :pkgname "purcell/ibuffer-vc")
        (:name yasnippet)
        (:name ack-mode
               :type github
               :pkgname "sudish/ack-mode.el"
               :load ("ack-mode.el"))
        (:name grep-in-project
               :type github
               :pkgname "ahobson/grep-in-project"
               :features grep-in-project)
        (:name find-file-in-project
               :type github
               :pkgname "ahobson/find-file-in-project"
               :load ("find-file-in-project.el"))
        (:name ruby-test-mode
               :type github
               :pkgname "ahobson/ruby-test-mode"
               :features ruby-test-mode)
        (:name project-anchor
               :type github
               :pkgname "ahobson/project-anchor"
               :load ("project-anchor.el"))
        (:name mac-classic-theme
               :type github
               :pkgname "ahobson/mac-classic-theme"
               :after (progn
                        (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/mac-classic-theme")
                        (load-theme 'mac-classic t)))))

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
