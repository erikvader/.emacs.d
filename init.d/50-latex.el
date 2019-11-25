(use-package tex-mode
  :ensure auctex
  :defer t
  :after-config
  ('flycheck
   (eriks/flycheck-add LaTeX-mode-hook (flycheck-mode 1)))
  :config
  (setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")
  :after-config
  ('evil
   (evil-set-initial-state 'latex-mode 'normal))
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
