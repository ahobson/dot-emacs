(setq my-user-config (concat user-emacs-directory "ahobson.el"))
(setq my-user-dir (concat user-emacs-directory "ahobson"))
(setenv "SEMGREP_SEND_METRICS" "off")

;; for Config

(define-derived-mode brazil-conf-mode
  conf-mode "BrazilConf"
  "Major mode for brazil config.")
(add-to-list 'auto-mode-alist '("Config\\'" . brazil-conf-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(brazil-conf-mode . "brazil-conf"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "barium")
                    :activation-fn (lsp-activate-on "brazil-conf")
                    :server-id 'barium)))
