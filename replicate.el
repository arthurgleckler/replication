(require 'cl-lib)

(defun replicated-maybe-commit-and-push ()
  "If `default-directory' is my Git-replicated directory and some
files have been updated, commit them and push."
  (interactive)
  (when (and (string-equal (expand-file-name default-directory)
                           (file-name-as-directory
                            (expand-file-name (getenv "r"))))
             (not (null (process-lines "git" "status" "--short"))))
    (condition-case nil
        (with-temp-message "Committing changes."
          (cl-flet ((run (command)
                      (assert (zerop (call-process-shell-command command)))))
            (run "git commit --all --message='Updated.'")
            (run "git push")))
      (error (message "Committing changes failed.")))))

(add-hook 'after-save-hook 'replicated-maybe-commit-and-push)