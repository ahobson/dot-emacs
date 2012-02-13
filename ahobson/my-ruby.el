;;
;; ruby customizations
(setq ruby-test-ruby-executables '("ruby"))

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
    '(progn
       (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)))

(defadvice switch-to-buffer (after my-rvm-switch-to-buffer nil activate)
  "When switching to a buffer in ruby mode, activate rvm."
  (when (and (eq 'ruby-mode major-mode)
             (not (string-match tramp-file-name-regexp buffer-file-name)))
    (rvm-activate-corresponding-ruby)))
(ad-activate 'switch-to-buffer)

(provide 'my-ruby)
