;;;; Replication via Git
;;;;
;;;; Copyright (c) MMV-MMXVI Arthur A. Gleckler.
;;;; Distributed under the terms of the GNU GPLv3
;;;; (<http://www.gnu.org/licenses/gpl-3.0.en.html>).

(defvar replication-directory nil
  "Use this directory for Git-based replication.")

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
	       ((exit) (message "Replication: %s" (string-trim string)))
	       (t (message "Replication %s status: %s"
			   name
			   status))))))))))

(defun replication-save-hook ()
  "If `default-directory' is my Git-replicated directory and some
files have been updated, commit them and push."
  (interactive)
  (unless replication-directory
    (error "Set `replication-directory' first."))
  (when (string-equal (expand-file-name default-directory)
		      (file-name-as-directory
		       (expand-file-name replication-directory)))
    (replication-maybe-commit-and-push)))

(defun replication-maybe-commit-and-push ()
  "If any files have been updated in my Git-replicated directory,
commit them and push."
  (interactive)
  (unless replication-directory
    (error "Set `replication-directory' first."))
  (let ((default-directory (file-name-as-directory
			    (expand-file-name replication-directory))))
    (if (null (process-lines "git" "status" "--short"))
	(message "No changes to commit.")
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
	     (set-process-sentinel
	      replication-process
	      (lambda (process string) (replication-git-push)))
	     (message "Previous <git push> still running."))
	    (t (replication-git-push))))))

(provide 'replication)