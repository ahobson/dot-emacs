(when (fboundp 'lsp)
  (add-hook 'go-mode-hook #'lsp))

(add-hook 'go-mode-hook '(lambda ()
                           (setq whitespace-line-column 400)))

(provide 'my-golang)
