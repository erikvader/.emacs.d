(use-package highlight-indent-guides
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
  :config
  (defalias 'fc-mode 'flycheck-mode)
  :init
  (cl-defun eriks/flycheck-activate-if-started-projectile ()
    "Activates `flycheck-mode' in the current buffer if another buffer
in the same projectile project also has flycheck enabled."
    (when-let (((not lsp-mode))
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
  ('prog-mode-hook 'eriks/flycheck-activate-if-started-projectile)
  ('(sh-mode-hook LaTeX-mode-hook minizinc-mode-hook)
   'flycheck-mode-on-safe)
  (nil (list
        (cl-defun eriks/flycheck-haskell-hook ()
          (when (derived-mode-p 'haskell-mode)
            (setq-local flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc))
            (setq-local flycheck-checker 'haskell-hlint)))

        (cl-defun eriks/flycheck-js-hook ()
          (when (derived-mode-p 'js-mode 'typescript-mode 'rjsx-mode)
            (when (functionp 'add-node-modules-path)
              (add-node-modules-path))))

        (cl-defun eriks/flycheck-python-hook ()
          (when (derived-mode-p 'python-mode)
            (setq-local flycheck-check-syntax-automatically '(save mode-enable))
            (setq-local flycheck-checker 'python-pylint)
            (setq-local flycheck-disabled-checkers '(python-mypy)))))))
