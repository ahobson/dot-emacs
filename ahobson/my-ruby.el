(require 'ruby-mode)
;;(require 'enh-ruby-mode)

;; disable lsp in favor of robe
;; (when (fboundp 'lsp)
;;   (add-hook 'ruby-mode-hook #'lsp))

;; (when (fboundp 'enh-ruby-mode)
;;   (add-to-list 'auto-mode-alist
;;                '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
;;   (setq enh-ruby-bounce-deep-indent t)
;;   (setq enh-ruby-deep-indent-paren t)
;;   (setq enh-ruby-hanging-brace-deep-indent-level 1)
;;   (setq enh-ruby-hanging-brace-indent-level 2)
;;   (setq enh-ruby-hanging-indent-level 2)
;;   (setq enh-ruby-hanging-paren-deep-indent-level 0)
;;   (setq enh-ruby-hanging-paren-indent-level 0)
;;   (define-key enh-ruby-mode-map (kbd "#") 'ruby-interpolate)
;;   (add-hook 'enh-ruby-mode-hook 'my-turn-on-smartparens)
;;   (add-hook 'enh-ruby-mode-hook 'my-turn-on-whitespace))

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

(defun my-om-ruby-console-hack ()
  "Create custom Gemfile and install pry"
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory
                                                   #'inf-ruby-console-match)))
    (with-temp-file "Gemfile_ahobson"
      (insert (format "%s\n%s\n%s\n"
                      "gem \"pry\""
                      "gem \"robe\", \"0.8.3\", github: \"dgutov/robe\""
                      "eval_gemfile \"Gemfile\""))))
  (copy-file "Gemfile.lock" "Gemfile_ahobson.lock" t)
  (with-environment-variables (("BUNDLE_GEMFILE" "Gemfile_ahobson"))
    (shell-command "bundle install")))

(defun my-inf-ruby-console-rails ()
  "Run inf-ruby-console-rails with RAILS_ENV set."
  (interactive)
  (let* ((default-directory (locate-dominating-file default-directory
                                                    #'inf-ruby-console-match))
         (inf-ruby-console-environment (inf-ruby-console-rails-env)))
    (with-environment-variables (("BUNDLE_GEMFILE" "Gemfile_ahobson")
                                 ("DISABLE_SPRING" "true")
                                 ("RAILS_ENV" inf-ruby-console-environment))
      (inf-ruby-console-rails default-directory))))

(defun my-robe-start ()
    "Start robe after calling my-inf-ruby-console-rails."
  (interactive)
  (my-inf-ruby-console-rails)
  ;; trying robe on codespaces, but it doesn't work well
  ;; (setq robe-port "25001")
  (robe-start))

(define-key ruby-mode-map (kbd "#") 'ruby-interpolate)

(require 'smartparens-config)
(require 'smartparens-ruby)

(defun my-turn-on-smartparens ()
  (smartparens-mode t))

(add-hook 'ruby-mode-hook 'my-turn-on-smartparens)
(add-hook 'ruby-mode-hook 'my-turn-on-whitespace)

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command)
                  (if lsp-ruby-lsp-use-bundler
                      (append '("bundle" "exec") command)
                    command)))))

(when (fboundp 'ruby-ts-mode)
  (define-key ruby-ts-mode-map (kbd "#") 'ruby-interpolate)
  (add-hook 'ruby-ts-mode-hook 'my-turn-on-smartparens)
  (add-hook 'ruby-ts-mode-hook 'my-turn-on-whitespace)
  (add-hook 'ruby-ts-mode-hook
            (lambda ()
              (setq-local flycheck-command-wrapper-function
                          (lambda (command)
                            (if lsp-ruby-lsp-use-bundler
                                (append '("bundle" "exec") command)
                              command))))))

(provide 'my-ruby)
;;; my-ruby.el ends here

