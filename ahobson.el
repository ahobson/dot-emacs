;; emacs starter kit customization
;; I'm not ready for full on ownership of emacs customization

;;
;; "global" requires
;;
(require 'epa-file)
(require 'grep)

;;
;; visuals
;;
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(setq browse-url-browser-function 'browse-default-macosx-browser)
(add-to-list 'default-frame-alist (cons 'width 100))

;;
;; keys
;;
(global-set-key (kbd "s-t") 'ffip)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'grep-in-project)
(global-set-key (kbd "C-c a") 'ack)

;;
;; misc?
;;
(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

(setq ack-root-directory-function
  (lambda ()
    (or ffip-project-root
        (ffip-project-root)
        (error "No project root found"))))

(add-hook 'prog-mode-hook 'esk-turn-on-whitespace)
(add-hook 'emacs-lisp-mode-hook 'esk-turn-on-paredit)

;; make these settings global and let dir local override
(setq whitespace-line-column 100)
(setq fill-column 100)
(setq js-indent-level 2)

(server-start)
;;
;; my customization broken out by file
;;
(require 'my-dir-local)
(require 'my-ruby)
