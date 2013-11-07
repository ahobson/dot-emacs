(setq gnus-secondary-select-methods
      '((nnimap "macarchive"
                (nnimap-address "localhost")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnimap "exchange"
                (nnimap-address "owa11.us.logicalis.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl))))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "ahobson@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "infloop.com")

(setq auth-source-save-behavior nil)
