;; (defun ensime-docker ()
;;   "Run ensime-server in docker and connect."
;;   (interactive)
;;   (let ((orig-bfn buffer-file-name))
;;     (condition-case ex
;;         (ensime-docker--maybe-update-and-start orig-bfn)
;;       ('error (error (format
;;                       "check that sbt is on your PATH and see the Troubleshooting Guide for further steps %s [%s]"
;;                       "http://ensime.github.io/editors/emacs/troubleshooting/" ex)))))  )

;; (defun ensime-docker--maybe-update-and-start (orig-buffer-file-name &optional host port)
;;   (if (and host port)
;;       ;; When both host and port are provided, we assume we're connecting to
;;       ;; an existing, listening server.
;;       (let* ((config-file (ensime-config-find orig-buffer-file-name))
;; 	     (config (ensime-config-load config-file))
;; 	     (cache-dir (file-name-as-directory (ensime--get-cache-dir config))))
;; 	(ensime--retry-connect nil host (lambda () port) config cache-dir))
;;     (let* ((config-file (ensime-config-find orig-buffer-file-name))
;;            (config (ensime-config-load config-file))
;;            (scala-version (plist-get config :scala-version))
;;            (assembly-file (ensime--assembly-file scala-version))
;;            (classpath-file (ensime--classpath-file scala-version)))
;;       (if (and (not (file-exists-p assembly-file))
;;                (ensime--classfile-needs-refresh-p classpath-file))
;;           (ensime--update-server scala-version `(lambda () (ensime--1 ,config-file)))
;;         (ensime-docker--1 config-file)))))

;; (defun* ensime-docker--1 (config-file)
;;   (when (and (ensime-source-file-p) (not ensime-mode))
;;     (ensime-mode 1))
;;   (let* ((config (ensime-config-load config-file))
;;          (root-dir (ensime--get-root-dir config) )
;;          (cache-dir (file-name-as-directory (ensime--get-cache-dir config)))
;;          (name (ensime--get-name config))
;;          (scala-version (plist-get config :scala-version))
;;          (server-env (or (plist-get config :server-env) ensime-default-server-env))
;;          (buffer (or (plist-get config :buffer) (concat ensime-default-buffer-prefix name)))
;;          (server-java (file-name-as-directory (ensime--get-java-home config)))
;;          (server-flags (or (plist-get config :java-flags) ensime-default-java-flags)))
;;     (make-directory cache-dir 't)

;;     (let* ((server-proc
;;             (ensime--maybe-start-server
;;              (generate-new-buffer-name (concat "*" buffer "*"))
;;              server-java scala-version server-flags
;;              (list* (concat "JDK_HOME=" server-java)
;;                     (concat "JAVA_HOME=" server-java)
;;                     server-env)
;;              config-file
;;              cache-dir))
;;            (host "127.0.0.1")
;;            (port-fn (lambda () (ensime--read-portfile
;;                              (concat cache-dir "/port")))))

;;       ;; Store the config on the server process so we can identify it later.
;;       (process-put server-proc :ensime-config config)
;;       (push server-proc ensime-server-processes)
;;       (ensime--retry-connect server-proc host port-fn config cache-dir))))
