(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :config
  (eriks/leader-def 'normal
    "f" 'flycheck-list-errors)
  :init
  (cl-defun eriks/flycheck-activate-if-started-projectile ()
    "Activates `flycheck-mode' in the current buffer if another buffer
in the same projectile project also has flycheck enabled."
    (when-let (((not (bound-and-true-p lsp-mode)))
               ((flycheck-may-enable-mode))
               (root (projectile-project-root))
               (cur-mode major-mode)
               ((cl-some (lambda (buf)
                           (with-current-buffer buf
                             (and (buffer-file-name buf)
                                  (eq major-mode cur-mode)
                                  flycheck-mode)))
                         (projectile-project-buffers root))))
      (flycheck-mode 1)))
  :gfhook
  ('(sh-mode-hook LaTeX-mode-hook minizinc-mode-hook)
   'flycheck-mode-on-safe)
  ('haskell-mode-hook (cl-defun eriks/flycheck-haskell-hook ()
                        (setq-local flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc))
                        (setq-local flycheck-checker 'haskell-hlint)
                        (eriks/flycheck-activate-if-started-projectile)))
  ('pyhon-mode-hook (cl-defun eriks/flycheck-python-hook ()
                      (setq-local flycheck-check-syntax-automatically '(save mode-enable))
                      (setq-local flycheck-checker 'python-pylint)
                      (setq-local flycheck-disabled-checkers '(python-mypy))
                      (eriks/flycheck-activate-if-started-projectile))))

(use-package apheleia
  :ensure t
  :diminish "Aph"
  :ghook 'emacs-lisp-mode-hook)

(use-package rainbow-delimiters
  :ensure t
  :ghook
  'prog-mode-hook
  'conf-mode-hook
  'TeX-mode-hook)
