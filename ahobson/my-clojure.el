(add-hook 'clojure-mode-hook 'my-turn-on-paredit)
(add-hook 'cider-repl-mode-hook 'my-turn-on-paredit)

(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)

(provide 'my-clojure)
