(defvar eriks/latex-autocompile-command nil
  "TeX command to auto compile with")
(make-variable-buffer-local 'eriks/latex-autocompile-command)

(defun eriks/latex-autocompile-toggle ()
  "Toggles whether the document should be compiled after each save or not.
Queries the command to use when toggling on."
  (interactive)
  (let ((is-enabled (memq 'eriks/latex-compile-on-save after-save-hook)))
    (if (not is-enabled)
        (let ((new-command (TeX-command-query (TeX-master-file nil nil t))))
          (when new-command
            (setq eriks/latex-autocompile-command new-command)
            (add-hook 'after-save-hook 'eriks/latex-compile-on-save nil t)))
      (remove-hook 'after-save-hook 'eriks/latex-compile-on-save t)
      (setq eriks/latex-autocompile-command nil)))
  (message "auto compilation is %s" (if eriks/latex-autocompile-command "on" "off")))

(defun eriks/latex-compile-on-save ()
  "Auto compile on save if enabled.
see `eriks/latex-autocompile-toggle'"
  (when eriks/latex-autocompile-command
    (TeX-command eriks/latex-autocompile-command 'TeX-active-master)))

(provide 'eriks-latex-autocompile)
