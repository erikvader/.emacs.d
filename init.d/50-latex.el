(use-package tex-mode
  :ensure auctex
  :defer t
  :config
  (setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")
  :custom
  (LaTeX-item-indent 0)
  :general
  ('LaTeX-mode-map
   "C-c e" 'TeX-error-overview)
  :gfhook
  ('LaTeX-mode-hook `(TeX-source-correlate-mode
                      ,(cl-defun latex-restore-paragraphs-hook ()
                         (dolist (i '(paragraph-start paragraph-separate))
                           (set i (default-value i))))
                      ,(cl-defun latex-run-prog-mode-hook ()
                         (run-hooks 'prog-mode-hook)))))

(use-package auctex-latexmk
  :ensure t
  :after tex-mode
  :config
  (auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package eriks-latex-autocompile
  :after tex-mode
  :general
  ('LaTeX-mode-map
   "C-c a" 'eriks/latex-autocompile-mode))
