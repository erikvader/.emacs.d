(defvar eriks/latex-autocompile-command nil
  "TeX command to auto compile with")
(make-variable-buffer-local 'eriks/latex-autocompile-command)

(defun eriks/latex-compile-on-save ()
  "Auto compile on save if enabled.
see `eriks/latex-autocompile-toggle'"
  (when (and eriks/latex-autocompile-command
             eriks/latex-autocompile-mode)
    (TeX-command eriks/latex-autocompile-command 'TeX-active-master)))

(define-minor-mode eriks/latex-autocompile-mode
  "Toggles whether the document should be compiled after each save or not.
Queries the command to use when toggling on."
  nil " LA" nil
  (unless (and (derived-mode-p 'latex-mode)
               (fboundp 'TeX-command-query)
               (fboundp 'TeX-master-file))
    (setq eriks/latex-autocompile-mode nil)
    (user-error "must be in latex-mode and have auctex installed"))
  (if eriks/latex-autocompile-mode
      (let ((new-command (condition-case err
                             (TeX-command-query (TeX-master-file nil nil t))
                           (quit (progn (setq eriks/latex-autocompile-mode nil)
                                        (signal (car err) (cdr err)))))))
        (setq eriks/latex-autocompile-command new-command)
        (add-hook 'after-save-hook 'eriks/latex-compile-on-save nil t))
    (remove-hook 'after-save-hook 'eriks/latex-compile-on-save t)
    (setq eriks/latex-autocompile-command nil)))

(provide 'eriks-latex-autocompile)
