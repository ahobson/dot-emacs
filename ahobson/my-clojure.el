(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'my-turn-on-paredit)
(add-hook 'cider-repl-mode-hook 'my-turn-on-paredit)

(provide 'my-clojure)
