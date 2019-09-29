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

(use-package frames-only-mode
  :ensure t
  :config
  (frames-only-mode 1)
  (defun kill-buffer-and-frame ()
    "Kills the current buffer, if successful then delete the frame."
    (interactive)
    (when (and
           (buffer-modified-p)
           (y-or-n-p "Current buffer is modified, save?"))
      (save-buffer))
    (when (kill-buffer)
      (delete-frame)))
  :custom
  (frames-only-mode-use-window-functions
   '(calendar
     report-emacs-bug
     checkdoc-show-diagnostics
     checkdoc
     undo-tree-visualize ;; undo-tree moves focus between frames on every action
     Custom-newline ;; customize actions window
     ))
  (frames-only-mode-kill-frame-when-buffer-killed-buffer-list
   '("*RefTeX Select*"
     "*Help*"
     "*Popup Help*"
     "*Completions*"
     "widget-choose" ;; actually close customize's action window
     ))
  (frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops nil)
  :general
  ("C-x C-0" 'kill-buffer-and-frame))

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
