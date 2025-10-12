(use-package rainbow-delimiters
  :ensure t
  :ghook 'prog-mode-hook)

(use-package eldoc
  :config
  (global-eldoc-mode)
  :diminish eldoc-mode)

;;TODO: how does this interact with tramp?
(use-package add-node-modules-path
  :ensure t
  :config
  (add-to-list 'safe-local-eval-forms '(add-node-modules-path)))

(use-package apheleia
  :ensure t
  :diminish "Aph"
  :general-config
  ("C-c C-f" 'apheleia-format-buffer))

(use-package projectile
  :ensure t
  :diminish
  :custom
  (projectile-current-project-on-switch 'keep)
  (projectile-ignored-project-function (cl-defun eriks/projectile-ignore-project (truename)
                                         "Ignore the sources of rust packages."
                                         (string-prefix-p (file-truename "~/.cargo") truename)))
  :config
  (put 'projectile-project-root 'safe-local-variable #'stringp)
  (projectile-mode 1)
  :general-config
  ('projectile-mode-map
   :prefix "C-c"
   "p" 'projectile-command-map))

;; TODO: use-package proced
