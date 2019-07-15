(use-package magit
  :ensure t
  :after evil
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "s" 'magit-status
   "b" 'magit-blame)
  :config
  (add-to-list 'evil-buffer-regexps
               '("^COMMIT_EDITMSG$" . insert))
  :gfhook ('magit-blame-mode-hook #'evil-emacs-state))

(use-package git-timemachine
  :ensure t
  :after evil
  :config
  (add-to-list 'evil-buffer-regexps
               '("^timemachine:" . emacs))
  :general
  ('normal
   :prefix eriks/leader
   "gt" 'git-timemachine)
  ('git-timemachine-mode-map
   "c" 'git-timemachine-show-commit))
