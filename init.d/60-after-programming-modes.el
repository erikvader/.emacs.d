(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :config
  (general-define-key :keymaps 'flycheck-mode-map flycheck-keymap-prefix nil)
  (eriks/leader-def 'normal
    "f" flycheck-command-map)
  (flycheck-add-next-checker 'python-pylint '(warning . python-pyright))
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
  ('python-mode-hook (cl-defun eriks/flycheck-python-hook ()
                       ;; NOTE: find a uv project with a venv and make flycheck use the
                       ;; executables in that venv
                       (when-let* ((venv-bin (eriks/python-find-venv-bin)))
                         (setq-local flycheck-python-pylint-executable (file-name-concat venv-bin "pylint")
                                     flycheck-python-pyright-executable (file-name-concat venv-bin "pyright")
                                     flycheck-python-mypy-executable (file-name-concat venv-bin "mypy")))

                       ;; NOTE: a temp file is created each time some checker, I have
                       ;; forgotten, is run, so this reduces the number of times it is
                       ;; invoked.
                       (setq-local flycheck-check-syntax-automatically '(save mode-enable))
                       (setq-local flycheck-checker 'python-pylint)
                       ;; NOTE: pylint can find its own configuration files, no need for
                       ;; flycheck to find them first using its own rules
                       (setq-local flycheck-pylintrc nil
                                   flycheck-python-mypy-config nil)
                       ;; NOTE: I don't like types in python and find it unnecessary to
                       ;; run this automatically after pylint
                       ;; (setq-local flycheck-disabled-checkers '(python-mypy))
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
