(add-hook 'clojure-mode-hook 'my-turn-on-paredit)
(add-hook 'cider-repl-mode-hook 'my-turn-on-paredit)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(provide 'my-clojure)
