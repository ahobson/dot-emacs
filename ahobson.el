;; emacs starter kit customization
;; I'm not ready for full on ownership of emacs customization

;;
;; "global" requires
;;
(require 'epa-file)

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

;;
;; misc?
;;
(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

;;
;; my customization broken out by file
;;
(require 'my-dir-local)
(require 'my-ruby)
