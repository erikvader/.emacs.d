(use-package dired
  :custom
  ;; NOTE: add --escape to handle weird characters, like newlines, better
  (dired-listing-switches "-l --classify --almost-all --human-readable --sort=version --time-style=long-iso --literal")
  (shell-command-guess-open "rifle")
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-collection-dired-setup)
  (evil-collection-wdired-setup)
  (put 'dired-find-alternate-file 'disabled nil)
  :gfhook
  'auto-revert-mode
  'dired-hide-details-mode
  :general-config
  ('dired-jump-map
   ;;NOTE: can't go down after a jump with this
   "j" nil)
  ('normal
   'dired-mode-map
   "gd" 'dired-kill-subdir
   "gh" 'dired-hide-subdir
   "<" 'dired-prev-subdir
   ">" 'dired-next-subdir))

(use-package dired-preview
  :ensure t)
