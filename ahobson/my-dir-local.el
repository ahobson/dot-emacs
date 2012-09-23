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

(setq rails-interesting-rpaths `("Gemfile" "README" "Rakefile" "app" "config" "db" "deploy"
                                 "lib" "public/stylesheets" "public/javascripts" "script"
                                 "spec" "test"))
(setq rails-ack-arguments `("--group" "--nopager" "--nocolor" "--ignore-dir=fixtures"
                            "-G"
                            ,(format "^(%s)" (mapconcat 'identity rails-interesting-rpaths "|"))))

(defun ffip-generate-project-files-with-ack (project-root)
  (let ((old-default-directory default-directory)
        (file-list nil))
    (setq default-directory project-root)
    (setq file-list
          (mapcar (lambda (rpath) (concat (file-name-as-directory project-root) rpath))
                  (split-string
                   (shell-command-to-string
                    (format "ack -f %s . | head -n %s"
                            (mapconcat (lambda (arg) (format "'%s'" arg)) ack-arguments " ")
                            ffip-limit)))))
    (setq default-directory old-default-directory)
    file-list))

(dir-locals-set-class-variables
 'rails-project
 `((nil . ((ffip-limit . 2048)
           (ffip-find-options . ,rails-ffip-find-options)
           (ffip-generate-files-function . ffip-generate-project-files-with-ack)
           (ack-arguments . ,rails-ack-arguments)
           (fill-column . 100)))))

(dir-locals-set-directory-class
  (expand-file-name "~/src/damballa/git/argus") 'rails-project)

(dir-locals-set-directory-class
  (expand-file-name "~/src/damballa/git/hadji") 'rails-project)

(dir-locals-set-directory-class
  (expand-file-name "~/src/damballa/git/dhq") 'rails-project)

(add-to-list 'safe-local-variable-values `(ffip-find-options . ,rails-ffip-find-options))
(add-to-list 'safe-local-variable-values
             '(ffip-generate-files-function . ffip-generate-project-files-with-ack))
(add-to-list 'safe-local-variable-values `(ack-arguments . ,rails-ack-arguments))

(provide 'my-dir-local)
