(when (fboundp 'lsp)
  (add-hook 'go-mode-hook #'lsp))

(provide 'my-golang)
