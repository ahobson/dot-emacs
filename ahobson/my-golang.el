;;; my-golang.el --- Summary
;;; Commentary:
;;; Code:
(when (fboundp 'lsp)
  (add-hook 'go-mode-hook #'lsp))

(add-hook 'go-mode-hook '(lambda ()
                           (setq whitespace-line-column 400)))

(defun my-go-test--get-current-subtest-info ()
  "Find subtest."
  (save-excursion
    (end-of-line)
    (let ((gotest-before-search (cadr (go-test--get-current-test-info))))
      (if (search-backward-regexp "suite.Run(\"\\([^,]*\\)\"" nil t)
          (let ((subtest-match (match-string-no-properties 1))
                (gotest (cadr (go-test--get-current-test-info))))
            (if (string= gotest gotest-before-search)
                (shell-quote-argument
                 (format ".*/%s/%s$" gotest
                         (replace-regexp-in-string "[[:space:]]" "_" subtest-match)))))))))

(defun my-gotest-current-test ()
  "Launch subtest if found, otherwise run current test."
    (interactive)
    (let ((subtest (my-go-test--get-current-subtest-info)))
      (if subtest
          (go-test--go-test (concat "-run " subtest " ."))
        (go-test-current-test))))

(eval-after-load 'go-mode
  '(define-key go-mode-map (kbd "C-c C-c") 'my-gotest-current-test))

(provide 'my-golang)
;;; my-golang.el ends here
