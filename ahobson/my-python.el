;;; my-python.el --- Summary
;;; Commentary:
;;; Code:

;; disabling for now as we try out nix
;; (setq python-shell-interpreter "docker-python")
;; (setq python-shell-interpreter-args "python -i")

;; (if (eq 'darwin system-type)
;;     (progn
;;       ;; this is needed to share files via boot2docker
;;       (setq my-python-tmp-dir (expand-file-name "~/.dockertmp"))
;;       (make-directory my-python-tmp-dir t)

;;       (defun my-python-shell-temp-file (orig &rest args)
;;         (let ((temporary-file-directory my-python-tmp-dir))
;;           (apply orig args)))

;;       (advice-add 'python-shell--save-temp-file
;;                   :around #'my-python-shell-temp-file)))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
(when (fboundp 'lsp-mode)
  (add-hook 'python-mode-hook #'lsp-deferred))
;;; my-python.el ends here

