(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (eriks/unset-key ivy-switch-buffer-map "C-k")
  :general
  ('ivy-minibuffer-map
   "<escape>" 'keyboard-escape-quit
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line
   "C-u" 'ivy-kill-line
   "C-w" 'ivy-backward-kill-word
   "C-b" 'ivy-scroll-down-command
   "C-f" 'ivy-scroll-up-command)
  ('ivy-switch-buffer-map
   "<delete>" 'ivy-switch-buffer-kill)
  :custom
  (ivy-use-selectable-prompt t)
  (ivy-wrap t))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :config
  (counsel-mode 1)
  :general
  ('counsel-mode-map
   [remap describe-bindings] nil
   [remap recentf-open-files] 'counsel-recentf
   "M-s" 'counsel-rg)
  ('normal 'counsel-mode-map
   :prefix eriks/leader
   "y" 'counsel-yank-pop))

(use-package swiper
  :ensure t
  :after (:and ivy counsel)
  :general
  ("C-s" 'counsel-grep-or-swiper))

(use-package counsel-projectile
  :ensure t
  :after (:and projectile counsel)
  :config
  (counsel-projectile-mode 1))
