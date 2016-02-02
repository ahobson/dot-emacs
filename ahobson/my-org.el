(setq org-directory "~/Dropbox/org")
(setq org-catch-invisible-edits 'smart)

(eval-after-load "org"
  '(require 'ox-md nil t))
