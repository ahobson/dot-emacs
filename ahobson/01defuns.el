;; stolen from the original emacs-starter-kit

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a byte-compiled lambda doesn't already exist in the list.

(defun my-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun my-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun my-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun my-turn-on-whitespace ()
  (whitespace-mode t)
  (setq whitespace-line-column 100))

(defun my-turn-on-paredit ()
  (paredit-mode t))

(defun my-turn-on-idle-highlight-mode ()
  (idle-highlight-mode t))

(defun my-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun my-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'my-local-column-number-mode)
(add-hook 'prog-mode-hook 'my-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'my-turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'my-pretty-lambdas)
(add-hook 'prog-mode-hook 'my-add-watchwords)
(add-hook 'prog-mode-hook 'my-turn-on-idle-highlight-mode)

(defun my-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun my-turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun my-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun my-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (my-indent-buffer)
  (my-untabify-buffer)
  (delete-trailing-whitespace))

;; Commands

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(defun my-c-basic-offset ()
  (setq c-basic-offset 2))
