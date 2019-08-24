(defun ahobson/truss-private-headers (url args)
  (let* ((truss-infra "https://github.com/trussworks/truss-infra/commits/master.atom")
         (private-token (string-trim-right
                         (shell-command-to-string
                          "security find-generic-password -a ahobson -s github-truss-infra -w"))))
    (when (member truss-infra url)
      (push (format "-HAuthorization: token %s" private-token) args)))
  args)
(add-hook 'elfeed-curl-arguments-hook 'ahobson/truss-private-headers)

