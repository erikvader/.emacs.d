(require 'dired-aux)

(define-advice counsel-find-file-extern (:override (x) diredified)
  "Execute an external command like `dired-do-shell-command' does."
  ;; NOTE: the dired things uses default-directory to determine if anything special is
  ;; needed to execute the command, basically TRAMP support. It is not set by counsel when
  ;; this action is executed, so it is manually set here.
  (let* ((default-directory (file-name-directory x))
         (cmd (dired-guess-shell-command (concat "Open " (file-name-nondirectory x) " ")
                                         (ensure-list x))))
    (dired-run-shell-command
     (dired-shell-stuff-it cmd (ensure-list x) nil))))

(provide 'eriks-counsel-extern)
