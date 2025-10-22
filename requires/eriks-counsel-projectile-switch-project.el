(defun eriks/counsel-projectile-switch-project ()
  "An alternative to `projectile-switch-project' that uses actions instead
of `projectile-commander'. It also uses the counsel version of all of
those when available."
  (interactive)
  (let ((projects (projectile-relevant-known-projects)))
    (unless projects
      (user-error "There are no know projects"))
    (ivy-read "Switch to project: " projects
              :require-match t
              :caller 'eriks/counsel-projectile-switch-project
              :action '(1
                        ("o" projectile-switch-project-by-name "default")
                        ("d" projectile-find-dir "dired")
                        ("s" eriks/projectile-switch-ripgrep-action "search files")
                        ("m" magit-status "magit")
                        ("k" eriks/projectile-switch-kill-buffers-action "kill buffers")
                        ;;TODO: how to refresh the list when called with `ivy-dispatching-call'? `ivy-quit-and-run'?
                        ("r" projectile-remove-known-project "remove from list")))))

(defun eriks/projectile-switch-ripgrep-action (project-to-switch)
  "Switch to a project by searching with ripgrep in all files."
  (counsel-rg nil project-to-switch))

(defun eriks/projectile-switch-kill-buffers-action (project-to-switch)
  "Kill the buffers of the selected project."
  (let ((projectile-switch-project-action #'projectile-kill-buffers))
    (projectile-switch-project-by-name project-to-switch)))

(provide 'eriks-counsel-projectile-switch-project)
