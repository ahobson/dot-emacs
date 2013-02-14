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
;(add-to-list 'default-frame-alist (cons 'font "-*-menlo-medium-r-normal--12-*-*-*-*-*-iso10646-1"))

;;
;; keys
;;
;; Distinguish between various Emacs ports to OS X
(cond
 ;; mac port
 ((boundp 'mac-carbon-version-string)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 120 :weight 'normal)
  (set-face-attribute 'bold nil :family "Source Code Pro" :height 120 :weight 'semi-bold)
  (setq mac-command-modifier 'super
        mac-option-modifier  'meta)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-f") 'isearch-repeat-forward)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-u") 'revert-buffer)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-n") 'make-frame-command)
  (global-unset-key [swipe-left])
  (global-unset-key [swipe-right])
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))))

(global-set-key (kbd "s-t") 'ffip)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'grep-in-project)
(global-set-key (kbd "C-c a") 'ack)

;;
;; misc?
;;
(mapc (lambda (dir) (add-to-list 'exec-path dir))
      `("/usr/local/sbin" "/usr/local/bin" ,(expand-file-name "~/bin")))

(setq ack-root-directory-function 'project-anchor-find-from-default-directory)

(add-hook 'project-anchor-find-hook 'project-anchor-find-by-file)
(add-hook 'project-anchor-find-hook 'project-anchor-find-with-mark)
(setq ffip-project-root-function 'project-anchor-find-from-default-directory)
(setq ack-mode-root-directory-function 'project-anchor-find-from-default-directory)

(add-hook 'prog-mode-hook 'esk-turn-on-whitespace)
(add-hook 'emacs-lisp-mode-hook 'esk-turn-on-paredit)
(add-hook 'clojure-mode-hook 'esk-turn-on-paredit)
;; Inserting a paren messes up the formatting, so disable for now
;;(add-hook 'ruby-mode-hook 'esk-turn-on-paredit) ;; I am CRAZY!

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
(require 'my-clojure)
