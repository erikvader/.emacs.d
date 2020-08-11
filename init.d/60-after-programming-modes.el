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
                        (when (flycheck-may-enable-mode)
                          (flycheck-mode 1)
                          (flycheck-select-checker 'haskell-hlint))))
  ('python-mode-hook (cl-defun eriks/flycheck-python-hook ()
                       ;; don't mess up lsp settings for flycheck
                       (when (and (not lsp-mode)
                                  (flycheck-may-enable-mode))
                         (flycheck-mode 1)
                         (flycheck-select-checker 'python-pylint)
                         (setq-local flycheck-check-syntax-automatically '(save mode-enable)))))
  :gfhook
  ('(c-mode-hook
     c++-mode-hook
     sh-mode-hook
     rjsx-mode-hook
     js-mode-hook
     LaTeX-mode-hook)
   'flycheck-mode-on-safe))

(use-package flycheck-rust
  :ensure t
  :gfhook
  ('rust-mode-hook (cl-defun eriks/flycheck-rust-hook ()
                     ;; don't mess up lsp settings for flycheck
                     (when (and (not lsp-mode)
                                (flycheck-may-enable-mode))
                       (flycheck-mode 1)
                       (flycheck-rust-setup)))))

(use-package flycheck-elixir
  :ensure t
  :gfhook
  ('elixir-mode-hook 'flycheck-mode-on-safe))
