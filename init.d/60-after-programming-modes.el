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
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :init
  (cl-defun eriks/flycheck-haskell-hook ()
    (when (flycheck-may-enable-mode)
      (flycheck-mode 1)
      (flycheck-select-checker 'haskell-hlint)))

  (cl-defun eriks/flycheck-python-hook ()
    ;; don't mess up lsp settings for flycheck
    (when (and (not lsp-mode)
               (flycheck-may-enable-mode))
      (flycheck-mode 1)
      (flycheck-select-checker 'python-pylint)
      (flycheck-disable-checker 'python-mypy)
      (setq-local flycheck-check-syntax-automatically '(save mode-enable))))

  (cl-defun eriks/flycheck-js-hook ()
    (when (functionp 'add-node-modules-path)
      (add-node-modules-path))
    (flycheck-mode-on-safe))

  (cl-defun eriks/flycheck-c-hook ()
    ;; don't mess up lsp settings for flycheck
    (when (and (not lsp-mode)
               (flycheck-may-enable-mode))
      (flycheck-mode 1)))

  :gfhook
  ('haskell-mode-hook 'eriks/flycheck-haskell-hook)
  ('python-mode-hook 'eriks/flycheck-python-hook)
  ('(js-mode-hook typescript-mode-hook rjsx-mode-hook)
   'eriks/flycheck-js-hook)
  ('(c-mode-hook c++-mode-hook)
   'eriks/flycheck-c-hook)
  ('(sh-mode-hook LaTeX-mode-hook minizinc-mode-hook)
   'flycheck-mode-on-safe)
  :config
  (eriks/hotfix
   'flycheck
   (defun eriks/flycheck-python-module-args-advice (_orig checker module-name)
     ":around advice for `flycheck-python-module-args'. pylint is
popping cwd from sys.path itself now. The pop is now
conditionally added to the command string.

See: https://github.com/PyCQA/pylint/pull/4164"
     (when (flycheck-python-needs-module-p checker)
       `("-c" ,(concat (when (not (eq checker 'python-pylint))
                         "import sys; sys.path.pop(0);")
                       "import runpy;"
                       (format "runpy.run_module(%S)" module-name)))))
   (advice-add 'flycheck-python-module-args
               :around
               #'eriks/flycheck-python-module-args-advice)))

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
