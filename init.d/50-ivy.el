(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)

  ;;TODO: Emacs doesn't change focus to the selected frame with
  ;;`ivy-occur-press-and-switch' for some reason. The function `select-frame' says it
  ;;should. See also `select-frame-set-input-focus'

  :general
  ('ivy-minibuffer-map
   "M-o" 'ivy-dispatching-call
   "C-M-o" 'ivy-dispatching-done
   "<escape>" 'keyboard-escape-quit
   "C-j" 'ivy-alt-done
   "TAB" 'ivy-partial)
  :custom
  (ivy-use-selectable-prompt t)
  (ivy-wrap t))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :custom
  (counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never --smart-case %s")
  :config
  (counsel-mode 1)
  :general
  ('counsel-mode-map
   [remap describe-bindings] nil
   [remap recentf-open-files] 'counsel-recentf)
  ('normal
   'counsel-mode-map
   :prefix eriks/leader
   "y" 'counsel-yank-pop))

(use-package eriks-counsel-grep
  :after counsel
  :general
  ('counsel-mode-map
   "M-s" 'eriks/counsel-grep))

(use-package swiper
  :ensure t
  :after ivy
  :general
  ('ivy-mode-map
   "C-s" 'swiper
   "C-S-s" 'swiper-all)
  ('(normal visual)
   :prefix eriks/leader
   "s" 'swiper-thing-at-point))

(use-package ivy-xref
  :ensure t
  :after ivy
  :config
  (eriks/frames-only-use-window-funcs 'ivy-xref-show-xrefs) ;; spawns a buffer that is immediately closed
  (if (< emacs-major-version 27)
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (setq xref-show-definitions-function #'ivy-xref-show-defs)))
