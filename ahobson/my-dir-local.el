;;
;; dir local customizations

;; somewhat frustrating that find needs exclusions and ack needs inclusions
;; find needs exclusions because -regex db matches things under .git
;; ack needs inclusions because --ignore-dir doesn't work with subdirectories
;; so --ignore-dir=help excludes paths other than public/help
(setq rails-ignore-rpaths `(".git" "etc" "public/help" "public/images"
                            "log" "solr" "test/fixtures" "tmp" "vendor"))
(setq rails-ffip-find-options (mapconcat (lambda (p) (format "-not -regex \".*/%s/.*\"" p))
                                         rails-ignore-rpaths " "))

(setq rails-interesting-rpaths `("Gemfile" "README" "Rakefile"
                                 "app" "config" "db" "deploy"
                                 "lib" "script" "spec"))
(setq rails-ack-arguments `("--nocolor" "-l"
                            "--ignore-dir=tmp"
                            "-g"
                            ,(format "(%s)" (mapconcat 'identity rails-interesting-rpaths "|"))))

(setq ffip-ack "ag")

(defun ffip-files-from-ack (project-root ack-cmd ack-args limit)
  (let ((old-default-directory default-directory)
        (file-list nil))
    (setq default-directory project-root)
    (setq ack-shell-command
          (format "%s %s . | head -n %s"
                  ack-cmd
                  (mapconcat (lambda (arg) (format "'%s'" arg)) ack-args " ")
                  limit))
    (setq file-list
          (mapcar
           (lambda (rpath) (concat (file-name-as-directory project-root) rpath))
           (split-string
            (shell-command-to-string ack-shell-command))))
    (setq default-directory old-default-directory)
    file-list))

(defun ffip-generate-project-files-with-ack (project-root)
  (ffip-files-from-ack project-root ffip-ack ack-arguments
                       ffip-limit))

(dir-locals-set-class-variables
 'rails-project
 `((nil . ((ffip-limit . 2048)
           (ffip-find-options . ,rails-ffip-find-options)
           (ffip-generate-files-function . ffip-generate-project-files-with-ack)
           (ack-arguments . ,rails-ack-arguments)
           (fill-column . 100)))))

;; (dir-locals-set-directory-class
;;  (expand-file-name "~/src/cyb/git/deerstand") 'rails-project)

(add-to-list 'safe-local-variable-values
             `(ffip-limit . 2048))
(add-to-list 'safe-local-variable-values
             `(ffip-find-options . ,rails-ffip-find-options))
(add-to-list 'safe-local-variable-values
             '(ffip-generate-files-function . ffip-generate-project-files-with-ack))
(add-to-list 'safe-local-variable-values `(ack-arguments . ,rails-ack-arguments))

;;
;; puppet
;;

(setq puppet-ack-arguments `("--group" "--nopager" "--nocolor" "-a"))
(dir-locals-set-class-variables
 'puppet-project
 `((nil . ((ffip-limit . 2048)
           (ffip-generate-files-function . ffip-generate-project-files-with-ack)
           (ack-arguments . ,puppet-ack-arguments)))))


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
