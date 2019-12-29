(use-package highlight-indent-guides
  :defer t
  :ensure t
  :diminish
  :custom
  (highlight-indent-guides-auto-character-face-perc 40)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-auto-top-character-face-perc 70)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :ghook 'python-mode-hook)

(use-package flycheck
  :ensure t
  :custom
  (flycheck-pylintrc (concat user-emacs-directory ".flycheck-pylintrc"))
  :gfhook
  ('haskell-mode-hook (cl-defun eriks/flycheck-haskell-hook ()
                        (flycheck-mode 1)
                        (flycheck-select-checker 'haskell-hlint)))
  ('python-mode-hook (cl-defun eriks/flycheck-python-hook ()
                       (flycheck-mode 1)
                       (flycheck-select-checker 'python-pylint)
                       (setq-local flycheck-check-syntax-automatically '(save mode-enable))))
  :ghook
  'c-mode-hook
  'c++-mode-hook
  'sh-mode-hook
  'rust-mode-hook
  'LaTeX-mode-hook)
