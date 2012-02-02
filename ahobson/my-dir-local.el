;;
;; dir local customizations

(setq rails-ignore-dirs (mapconcat 'identity '(
                                               "-not -regex \".*/.git/.*\""
                                               "-not -regex \".*/etc/.*\""
                                               "-not -regex \".*/public/help/.*\""
                                               "-not -regex \".*/public/images/.*\""
                                               "-not -regex \".*/log/.*\""
                                               "-not -regex \".*/solr/.*\""
                                               "-not -regex \".*/tmp/.*\""
                                               "-not -regex \".*/vendor/.*\""
                                               ) " "))
(dir-locals-set-class-variables
 'rails-project
 `((nil . ((ffip-limit . 2048)
           (ffip-find-options . ,rails-ignore-dirs)
           (whitespace-line-column . 100)))))

(dir-locals-set-class-variables
 'emacs-init
 '((nil . ((ffip-limit . 512)
           (ffip-find-options . "")
           (whitespace-line-column . 100)))))
     
(dir-locals-set-directory-class
  (expand-file-name "~/src/damballa/git/argus") 'rails-project)

(dir-locals-set-directory-class
  (expand-file-name "~/src/damballa/git/hadji") 'rails-project)

(dir-locals-set-directory-class
  (expand-file-name "~/src/git/dot-emacs") 'emacs-init)

(add-to-list 'safe-local-variable-values `(ffip-find-options . ,rails-ignore-dirs))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 100))

(provide 'my-dir-local)
