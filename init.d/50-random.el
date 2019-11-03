(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/theme nil)
  :config
  (sml/setup))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode 1)
  :general
  ('projectile-mode-map
   :prefix "C-c"
   "p" 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-expand-only-for-last-commands '(self-insert-command org-self-insert-command))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ibuffer
  :defer t
  :general
  ("C-x C-b" 'ibuffer))

(use-package dumb-jump
  :ensure t
  :defer t
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (advice-add 'dumb-jump-go :before (cl-defun eriks/dumb-jump-go-evil-advice (&rest r)
                                      (evil-set-jump)))
  :general
  ('normal
   "gd" 'dumb-jump-go
   "gD" 'dumb-jump-quick-look))
