(require 'ruby-mode)
;;(require 'enh-ruby-mode)
(when (fboundp 'lsp-mode)
  (add-hook 'ruby-mode-hook #'lsp))

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

;; begin from starter-kit-ruby
;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

;; Rinari (Minor Mode for Ruby On Rails)
(setq rinari-major-modes
      (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
            'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; end from starter-kit-ruby

;; ruby customizations
(setq ruby-test-ruby-executables '("ruby"))
(setq ruby-test-rspec-executables '("rspec"))
;;(setq ruby-deep-indent-paren nil)


(setq redenv-global-env-prefix
      (expand-file-name (concat "~/.redenv/" my-os-version)))

(defadvice redenv-use (after my-redenv-use nil activate)
  "When activating redenv, also set LOCAL_VERSION env."
  (if (and (fboundp 'vc-git-branches)
           (not (equal "master" (car (vc-git-branches)))))
      (setenv "LOCAL_VERSION" (car (vc-git-branches)))
    (setenv "LOCAL_VERSION" nil)))
(ad-activate 'redenv-use)

(defadvice inf-ruby-console-auto (before activate-redenv-for-robe activate)
  (redenv-activate-corresponding-ruby))

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(define-key ruby-mode-map (kbd "#") 'ruby-interpolate)
(define-key ruby-mode-map (kbd "s-.") 'xref-find-definitions)

(require 'smartparens-config)
(require 'smartparens-ruby)

(defun my-turn-on-smartparens ()
  (smartparens-mode t))

(add-hook 'ruby-mode-hook 'my-turn-on-smartparens)
(add-hook 'ruby-mode-hook 'my-turn-on-whitespace)

(defadvice switch-to-buffer (after my-redenv-switch-to-buffer nil activate)
  "When switching to a buffer in ruby mode, activate redenv."
  (when (and (or (eq 'ruby-mode major-mode)
                 (eq 'enh-ruby-mode major-mode))
             (not (string-match tramp-file-name-regexp buffer-file-name)))
    (redenv-activate-corresponding-ruby)
    (when (and (fboundp 'enh-ruby-mode)
               (eq 'enh-ruby-mode major-mode)
               (first redenv--current-ruby-binary-path))
      (setq enh-ruby-program
            (concat (first redenv--current-ruby-binary-path) "ruby")))))
(ad-activate 'switch-to-buffer)

(provide 'my-ruby)
