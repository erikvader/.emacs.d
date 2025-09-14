(use-package tex-mode
  :ensure auctex
  :config
  ;;TODO: this doesn't work anymore for some reason after removing evil-latex-textobjects.
  ;;Thing are loading in the incorrect order it seems. Why isn't auctex used as the first
  ;;argument to use-package?
  ;; (setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "PDF Tools")
  (evil-set-initial-state 'tex-mode 'normal)
  (defun eriks/latex-flyspell-mode-enable ()
    (interactive)
    (setq-local ispell-local-dictionary "british")
    (flyspell-mode)
    (setq ispell-parser 'tex))
  :custom
  (LaTeX-item-indent 0)
  :general-config
  ('TeX-mode-map
   "C-c e" 'TeX-error-overview)
  :gfhook
  ('TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  ('TeX-mode-hook '(TeX-source-correlate-mode eriks/run-prog-mode-hooks)))

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
  :general-config
  ('(TeX-mode-map bibtex-mode-map)
   "C-c a" 'eriks/latex-autocompile-mode))

;;TODO: https://github.com/iyefrat/evil-tex
