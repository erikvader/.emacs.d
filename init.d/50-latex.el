(use-package tex-mode
  :ensure auctex
  :defer t
  :config
  (setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "PDF Tools")
  (evil-set-initial-state 'tex-mode 'normal)
  (eriks/frames-only-use-window-funcs 'TeX-next-error)
  (defun eriks/latex-flyspell-mode-enable ()
    (interactive)
    (setq-local ispell-local-dictionary "british")
    (flyspell-mode)
    (setq ispell-parser 'tex))
  :custom
  (LaTeX-item-indent 0)
  :general
  ('TeX-mode-map
   "C-c e" 'TeX-error-overview)
  :gfhook
  ('TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  ('TeX-mode-hook `(TeX-source-correlate-mode
                    ,(cl-defun latex-run-prog-mode-hook ()
                       ;;TODO: do I want to do it this way?
                       ;; why not add all hooks I want run explicitly?
                       (run-hooks 'prog-mode-hook)))))

(use-package auctex-latexmk
  :ensure t
  :after tex-mode
  :config
  (auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package bibtex
  :config
  (evil-set-initial-state 'bibtex-mode 'normal))

(use-package eriks-latex-autocompile
  :after tex-mode
  :general
  ('(TeX-mode-map bibtex-mode-map)
   "C-c a" 'eriks/latex-autocompile-mode))
