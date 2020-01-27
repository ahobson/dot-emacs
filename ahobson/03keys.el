;; Distinguish between various Emacs ports to OS X
(cond
 ;; mac port
 ((setq mac-command-modifier 'super
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
  (global-unset-key (kbd "C-z"))
  (global-unset-key [swipe-left])
  (global-unset-key [swipe-right])
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))))

;; Search using ack
(global-set-key (kbd "C-c a") 'projectile-ag)
;; or grep
(global-set-key (kbd "C-c g") 'grep-in-project)

;; Use smex
(global-set-key (kbd "M-x") 'smex)

;; finding files
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; flyspell mouse
(require 'flyspell)
(define-key flyspell-mouse-map [s-mouse-1] #'flyspell-correct-word)
