(setq org-directory "~/Dropbox/org")
(setq org-catch-invisible-edits 'smart)

(eval-after-load "org"
  '(require 'ox-md nil t))

(defun my-org-file ()
  "Switch to org file"
  (interactive)
  (let ((default-directory
          (concat (file-name-as-directory org-directory)
                  (ido-completing-read "org: "
                                       (directory-files org-directory nil "^[^.].*")))))
    (ido-find-file)))

(global-set-key (kbd "C-c C-0") 'my-org-file)
