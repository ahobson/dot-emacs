;;
;; based on code by Scott Frazer
;; http://scottfrazersblog.blogspot.com/2009/12/emacs-better-ido-flex-matching.html
;;

(defun my-ido-make-index-hash-table (str)
  (let ((char-lookup (make-hash-table :test 'equal)))
    ;; Make hash table of all characters with their corresponding indexes
    (let ((chars (split-string (if ido-case-fold (downcase str) str) "" t))
          (idx 0)
          elt)
      (dolist (char chars)
        (setq elt (gethash char char-lookup))
        (if elt
            (push idx elt) ;; It's important that the indexes are in descending order
          (setq elt (list idx)))
        (puthash char elt char-lookup)
        (setq idx (1+ idx))))
    char-lookup))

(defun my-ido-fuzzy-weight-list (str items)
  (let ((char-lookup (my-ido-make-index-hash-table str))
        corr matches)
    (dolist (item items)
      (setq corr (my-ido-match-get-correlation str char-lookup (ido-name item)))
      (when corr
        (push (cons item corr) matches)))
    matches))

(defun my-ido-fuzzy-match (str items)
  "Better ido fuzzy matching."
  (let ((str-len (length str)))
    (if (= 0 str-len)
        (reverse items)
      (let ((matches (my-ido-fuzzy-weight-list str items)))
        ;; Sort matches and return
        (mapcar 'car (if ido-rotate
                         matches
                       (sort matches (lambda (x y)
                                       (let ((xval (cdr x))
                                             (yval (cdr y)))
                                         (if (equal xval yval)
                                             (< (length (car x)) (length (car y)))
                                           (> (cdr x) (cdr y))))))))))))

(defvar my-ido-separator-chars '("-" "_")
  "Separator chars for extra weighting.")

(defun my-ido-match-get-correlation (str search-hash item)
  "Get the correlation for this item."
  (let ((item-chars (split-string (if ido-case-fold (downcase item) item) "" t))
        (str-chars (split-string (if ido-case-fold (downcase str) str) "" t))
        (abbrev-chars nil)
        (prev-char nil)
        (prev-match-idxs nil)
        (longest-match 0)
        (current-match 0)
        (corr 0))
    ;; matching first char gets mega points
    (if (eq 0 (car (last (gethash (first item-chars) search-hash))))
        (setq corr (+ 5 corr)))
    (dolist (char item-chars)
      (if (find prev-char my-ido-separator-chars :test 'equal)
          (push char abbrev-chars))
      (let ((idxs (gethash char search-hash)))
        (setq prev-match-idxs
              (if prev-match-idxs
                  (intersection idxs (mapcar '1+ prev-match-idxs))
                idxs))
        (if prev-match-idxs
            (setq current-match (1+ current-match))
          (setq current-match 0))
        (setq longest-match (max longest-match current-match)))
      (setq prev-char char))
    (+ corr longest-match (* 5 (count t (mapcar* #'equal str-chars abbrev-chars))))))

(defvar my-ido-use-fuzzy-match nil
  "*Use my-ido-fuzzy-match for ido matching.")

(defvar my-ido-max-fuzzy-match 1024
  "*Only do fuzzy matching on fewer than this many entries.")

(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  "Choose between the regular ido-set-matches-1 and my-ido-fuzzy-match"
  (if (and my-ido-use-fuzzy-match (> my-ido-max-fuzzy-match (length (ad-get-arg 0))))
      (setq ad-return-value (my-ido-fuzzy-match ido-text (ad-get-arg 0)))
    ad-do-it))
