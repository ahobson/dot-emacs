;;
;; dir local customizations

(dir-locals-set-class-variables
 'rails-project
 `((nil . ((fill-column . 100)))))

(defun my-dir-project ()
  "Switch to project in project dir"
  (interactive)
  (let ((default-directory
          (concat (file-name-as-directory "~/src/truss/git")
                  (ido-completing-read "project: "
                                       (directory-files "~/src/truss/git" nil "^[^.].*")))))
    (ido-find-file)))

(global-set-key (kbd "C-c 8") 'my-dir-project)

(provide 'my-dir-local)
