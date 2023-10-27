(require 'ruby-mode)
;;(require 'enh-ruby-mode)

;; disable lsp in favor of robe
;; (when (fboundp 'lsp)
;;   (add-hook 'ruby-mode-hook #'lsp))

(when (fboundp 'enh-ruby-mode)
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-deep-indent-paren t)
  (setq enh-ruby-hanging-brace-deep-indent-level 1)
  (setq enh-ruby-hanging-brace-indent-level 2)
  (setq enh-ruby-hanging-indent-level 2)
  (setq enh-ruby-hanging-paren-deep-indent-level 0)
  (setq enh-ruby-hanging-paren-indent-level 0)
  (define-key enh-ruby-mode-map (kbd "#") 'ruby-interpolate)
  (add-hook 'enh-ruby-mode-hook 'my-turn-on-smartparens)
  (add-hook 'enh-ruby-mode-hook 'my-turn-on-whitespace))

;; ruby customizations
(setq ruby-test-ruby-executables '("ruby"))
(setq ruby-test-rspec-executables '("rspec"))
;;(setq ruby-deep-indent-paren nil)

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert-char (string-to-char "#") (or current-prefix-arg 1))
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(defun my-inf-ruby-console-rails ()
  "Run inf-ruby-console-rails with RAILS_ENV set."
  (interactive)
  (let* ((default-directory (locate-dominating-file default-directory
                                                    #'inf-ruby-console-match))
         (inf-ruby-console-environment (inf-ruby-console-rails-env))
         (process-environment (cons (format "RAILS_ENV=%s" inf-ruby-console-environment)
                                    process-environment)))
    (inf-ruby-console-rails default-directory)))

(define-key ruby-mode-map (kbd "#") 'ruby-interpolate)

(require 'smartparens-config)
(require 'smartparens-ruby)

(defun my-turn-on-smartparens ()
  (smartparens-mode t))

(add-hook 'ruby-mode-hook 'my-turn-on-smartparens)
(add-hook 'ruby-mode-hook 'my-turn-on-whitespace)

(provide 'my-ruby)
;;; my-ruby.el ends here

