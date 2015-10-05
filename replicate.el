(defvar replication-process nil)

(defun replication-git-push ()
  "Start a <git push> process in the background."
  (interactive)
  (let ((name "*git-push*"))
    (setq replication-process
	  (start-process-shell-command name name "git push"))
    (set-process-sentinel
     replication-process
     (lexical-let ((name name))
       (lambda (process string)
	 (let ((status (process-status process)))
	   (unless (eq status 'run)
	     (case status
	       ((exit) (message "Replication successful."))
	       (t (message "Replication %s status: %s"
			   name
			   status))))))))))

(defun replication-maybe-commit-and-push ()
  "If `default-directory' is my Git-replicated directory and some
files have been updated, commit them and push."
  (interactive)
  (when (and (string-equal (expand-file-name default-directory)
			   (file-name-as-directory
			    (expand-file-name (getenv "r"))))
	     (not (null (process-lines "git" "status" "--short"))))
    (condition-case nil
	(with-temp-message "Committing changes."
	  (assert
	   (zerop
	    (call-process-shell-command
	     "git commit --all --message='Updated.'"))))
      (error (message "Committing changes failed.")))
    (cond ((and replication-process
		(not (eq (process-status replication-process)
			 'exit)))
	   (set-process-sentinel	; <> Is there a race here?
	    replication-process
	    (lambda (process string) (replication-git-push)))
	   (message "Previous <git push> still running."))
	  (t (replication-git-push)))))