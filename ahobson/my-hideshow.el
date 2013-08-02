(defun ahobson/ruby-end-of-block ()
  (interactive)
  (if (re-search-forward "\\( \\|;\\)end$" (line-end-position) t)
      (backward-char 3)
      (ruby-end-of-block)))

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ahobson/ruby-end-of-block)) nil))

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; workaround for http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14504
(unless (keymap-parent lisp-mode-shared-map)
  (set-keymap-parent lisp-mode-shared-map prog-mode-map))

(defun ahobson/hs-toggle-all ()
  (interactive)
  (if (> (point-max) (next-overlay-change (point-min)))
      (hs-show-all)
    (hs-hide-all)))

(define-key prog-mode-map (kbd "C-=") 'hs-toggle-hiding)
(define-key prog-mode-map (kbd "C-+") 'ahobson/hs-toggle-all)

(provide 'my-hideshow)
