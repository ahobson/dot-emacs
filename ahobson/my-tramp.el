;;; my-tramp.el --- Summary                               -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; from
;; https://github.com/emacs-lsp/lsp-mode/issues/2709#issuecomment-1475039310

(defun lsp-tramp-connection-over-ssh-port-forwarding (command)
  "Like lsp-tcp-connection, but use SSH portforwarding to run COMMAND."
  (list
   :connect (lambda (filter sentinel name environment-fn _workspace)
              (let* ((host "localhost")
                     (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (with-parsed-tramp-file-name buffer-file-name nil
                                (message "[tcp/ssh hack] running LSP %s on %s / %s" command host localname)
                                (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock" lsp-port))
                                       (command (list
                                                 "ssh"
                                                 ;; "-vvv"
                                                 "-L" (format "%s:%s" lsp-port unix-socket)
                                                 host
                                                 "socat"
                                                 (format "unix-listen:%s" unix-socket)
                                                 (format "system:'\"tramp-lsp %s %s\"'" (file-name-directory localname) command)
                                                 )))
                                  (message "using local command %s" command)
                                  command)))
                     (final-command (if (consp command) command (list command)))
                     (_ (unless (executable-find (cl-first final-command))
                          (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                     (process-environment
                      (lsp--compute-process-environment environment-fn))
                     (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                         :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
                     (tcp-proc (progn
                                 (sleep-for 1) ; prevent a connection before SSH has run socat. Ugh.
                                 (lsp--open-network-stream host lsp-port (concat name "::tcp")))))

                ;; TODO: Same :noquery issue (see above)
                (set-process-query-on-exit-flag proc nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (lambda () t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection-over-ssh-port-forwarding "gopls")
                  :major-modes '(go-mode)
                  :remote? t
                  :server-id 'gopls-remote))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection-over-ssh-port-forwarding "typescript-language-server --stdio")
                  :major-modes '(js-mode typescript-mode web-mode)
                  :remote? t
                  :server-id 'tsls-remote))

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

