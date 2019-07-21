(defvar eriks/latex-autocompile nil
  "Whether the current buffer's after-save-hook should compile the
  document")
(make-variable-buffer-local 'eriks/latex-autocompile)
(defvar eriks/latex-autocompile-command nil
  "TeX command to auto compile with")
(make-variable-buffer-local 'eriks/latex-autocompile-command)

(defun eriks/latex-autocompile-toggle ()
  "Toggles whether the document should be compiled after each save or not.
Queries the command to use when toggling on."
  (interactive)
  (setq eriks/latex-autocompile-command nil)
  (when (not eriks/latex-autocompile)
    (setq eriks/latex-autocompile-command (TeX-command-query (TeX-master-file nil nil t))))
  (setq eriks/latex-autocompile (and (not eriks/latex-autocompile)
                                     eriks/latex-autocompile-command))
  (message "auto compilation is %s" (if eriks/latex-autocompile "on" "off")))

(defun eriks/latex-compile-on-save ()
  "Auto compile on save if enabled.
see `eriks/latex-autocompile-toggle'"
  (when (and eriks/latex-autocompile
             eriks/latex-autocompile-command)
    (TeX-command eriks/latex-autocompile-command 'TeX-active-master)))

(define-minor-mode eriks-latex-autocompile-mode
  "Enables the ability to autocompile the document on each save.
  Activate with `eriks/latex-autocompile-toggle'"
  nil
  " AutoCmpl"
  (make-sparse-keymap)
  (if eriks-latex-autocompile-mode
      (add-hook 'after-save-hook 'eriks/latex-compile-on-save nil t)
    (remove-hook 'after-save-hook 'eriks/latex-compile-on-save t)))

(provide 'eriks-latex-autocompile)
