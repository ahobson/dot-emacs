;;

(setq enh-ruby-program
      (let ((ruby-bin-dir (expand-file-name "~/.rvm/bin")))
        (concat (file-name-as-directory ruby-bin-dir)
                (car (reverse
                      (sort
                       (delq nil
                             (mapcar (lambda (f)
                                       (when (string-match "^ruby-1.9[^@]*$" f) f))
                                     (directory-files ruby-bin-dir))) 'string<))))))
;; begin from starter-kit-ruby
;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

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
(setq ruby-test-rspec-executables '("brspec"))
;;(setq ruby-deep-indent-paren nil)

(defadvice rvm-use (after my-rvm-use nil activate)
  "When activating rvm, also set LOCAL_VERSION env."
  (if (and (fboundp 'vc-git-branches) (not (equal "master" (car (vc-git-branches)))))
      (setenv "LOCAL_VERSION" (car (vc-git-branches)))
    (setenv "LOCAL_VERSION" nil)))
(ad-activate 'rvm-use)

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

;; (defface ruby-string-variable-face
;;   '((((class color) (background dark))
;;      (:foreground "darkgray"))
;;     (((class color) (background light))
;;      (:foreground "lightgray"))
;;     (t (:inverse-video t)))
;;   "Face used to visualize variable interpolation inside a string."
;;   :group 'ruby)

;; (defvar ruby-string-variable-face    'ruby-string-variable-face
;;   "Face name to use for ruby interpolated strings.")

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(require 'smartparens-config)
(require 'smartparens-ruby)

(defun my-turn-on-smartparens ()
  (smartparens-mode t))

(add-hook 'enh-ruby-mode-hook 'my-turn-on-smartparens)

(add-to-list 'interpreter-mode-alist
             '("ruby1.9.1" . ruby-mode))

(defadvice switch-to-buffer (after my-rvm-switch-to-buffer nil activate)
  "When switching to a buffer in ruby mode, activate rvm."
  (when (and (eq 'ruby-mode major-mode)
             (not (string-match tramp-file-name-regexp buffer-file-name)))
    (rvm-activate-corresponding-ruby)))
(ad-activate 'switch-to-buffer)

(provide 'my-ruby)
