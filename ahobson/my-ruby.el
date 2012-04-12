;;
;; ruby customizations
(setq ruby-test-ruby-executables '("ruby"))
(setq ruby-test-rspec-executables '("rspec"))
;;(setq ruby-deep-indent-paren nil)

(defadvice rvm-use (after my-rvm-use nil activate)
  "When activating rvm, also set LOCAL_VERSION env."
  (if (and (fboundp 'vc-git-branches) (not (equal "master" (car (vc-git-branches)))))
      (setenv "LOCAL_VERSION" (car (vc-git-branches)))))
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

(defface ruby-string-variable-face
  '((((class color) (background dark))
     (:foreground "darkgray"))
    (((class color) (background light))
     (:foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize variable interpolation inside a string."
  :group 'ruby)

(defvar ruby-string-variable-face    'ruby-string-variable-face
  "Face name to use for ruby interpolated strings.")

(eval-after-load 'ruby-mode
    '(progn
       (setq ruby-font-lock-keywords
             (mapcar (lambda (entry)
                       (if (and (sequencep (cdr entry))
                                (equal ?# (string-to-char (car entry)))
                                (equal 3 (length (cdr entry))))
                           `(,(car entry) 0 ruby-string-variable-face t)
                         entry)) ruby-font-lock-keywords))
        ;; (font-lock-add-keywords 'ruby-mode
        ;;                         '(("do\\|{\s*|\\(\\([\\w|_]+\\),?\\)+|" 1 font-lock-variable-name-face)))
       (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)))

(add-to-list 'interpreter-mode-alist
             '("ruby1.9.1" . ruby-mode))

(defadvice switch-to-buffer (after my-rvm-switch-to-buffer nil activate)
  "When switching to a buffer in ruby mode, activate rvm."
  (when (and (eq 'ruby-mode major-mode)
             (not (string-match tramp-file-name-regexp buffer-file-name)))
    (rvm-activate-corresponding-ruby)))
(ad-activate 'switch-to-buffer)

(provide 'my-ruby)
