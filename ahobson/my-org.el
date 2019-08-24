(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org")
(setq org-catch-invisible-edits 'smart)
(setq org-agenda-files (concat (file-name-as-directory org-directory) "agenda-files"))

(eval-after-load "org"
  '(require 'ox-md nil t))

(defun my-org-file ()
  "Switch to org file"
  (interactive)
  (let ((default-directory
          (concat (file-name-as-directory org-directory)
                  (ido-completing-read
                   "org: "
                   (mapcar 'car
                           (seq-filter (lambda (x) (nth 1 x))
                                       (directory-files-and-attributes
                                        org-directory nil "^[^.].*")))))))
    (ido-find-file)))

(define-prefix-command 'my-org-map)
(global-set-key (kbd "C-c C-0") 'my-org-map)
(define-key my-org-map (kbd "f") 'my-org-file)
(define-key my-org-map (kbd "t") 'org-todo-list)
(define-key my-org-map (kbd "a") 'org-agenda-list)
