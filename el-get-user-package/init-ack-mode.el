(setq ack-mode-program-name (or (executable-find "ag")
                                (executable-find "ack-grep")
                                (executable-find "ack")))
