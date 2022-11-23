;;; my-tramp.el --- Summary
;;; Commentary:
;;; Code:

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "/Users/ahobson/bin/tramp-gopls")
;;                   :major-modes '(go-mode)
;;                   :remote? t
;;                   :server-id 'gopls-remote))

(with-eval-after-load "tramp"
  (add-to-list 'tramp-remote-path (expand-file-name "~/.nix-profile/bin"))
  (add-to-list 'tramp-remote-path (expand-file-name "~/bin")))

;;(customize-set-variable 'tramp-encoding-shell "/bin/zsh")

;;; https://github.com/eush77/dotfiles/blob/ace2b1dc0f229b4897d60728a320a8f88722e0c4/emacs/.emacs.d/config/config-direnv.el#L28-L52

(defvar my-direnv-enabled-hosts '("fiddler.local")
  "List of remote hosts to use Direnv on.
Each host must have `direnv' executable accessible in the default
environment.")

;; (defun tramp-sh-handle-start-file-process@my-direnv (args)
;;   "Enable Direnv for hosts in `my-direnv-enabled-hosts'."
;;   (with-parsed-tramp-file-name (expand-file-name default-directory) nil
;;     (if (member host my-direnv-enabled-hosts)
;;         (pcase-let ((`(,name ,buffer ,program . ,args) args))
;;           `(,name
;;             ,buffer
;;             "/tmp/direnv"
;;             "exec"
;;             ,localname
;;             ,program
;;             ,@args))
;;       args)))

;; (with-eval-after-load "tramp-sh"
;;   (advice-add 'tramp-sh-handle-start-file-process
;;               :filter-args #'tramp-sh-handle-start-file-process@my-direnv))

(defun tramp-handle-start-file-process@my-direnv (args)
  "Enable Direnv for hosts in `my-direnv-enabled-hosts'."
  (with-parsed-tramp-file-name (expand-file-name default-directory) nil
    (if (member host my-direnv-enabled-hosts)
        (progn
          (pcase-let ((`(,name ,buffer ,program . ,args) args))
            `(,name
              ,buffer
              "~/bin/tramp"
              ,program
              ,@args)))
      args)))

(with-eval-after-load "tramp"
  (advice-add 'tramp-handle-start-file-process
              :filter-args #'tramp-handle-start-file-process@my-direnv))

(provide 'my-tramp)

;;; my-tramp.el ends here

