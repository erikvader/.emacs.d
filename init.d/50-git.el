(use-package magit
  :ensure t
  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  (magit-diff-refine-hunk t)
  (magit-commit-show-diff nil)
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "s" 'magit-status
   "b" 'magit-blame
   "f" 'magit-find-file)
  :config
  (add-to-list 'evil-buffer-regexps
               '("^COMMIT_EDITMSG$" . insert))
  :gfhook ('magit-blame-mode-hook #'evil-emacs-state))

(use-package git-timemachine
  :ensure t
  :config
  (add-to-list 'evil-buffer-regexps
               '("^timemachine:" . emacs))
  :general
  ('normal
   :prefix eriks/leader
   "gt" 'git-timemachine))

(use-package git-walktree
  :ensure t
  :config
  (evil-set-initial-state 'git-walktree-mode 'emacs)
  (defun eriks/git-walktree-show-commit ()
    (interactive)
    (magit-show-commit git-walktree-current-commitish))
  :gfhook
  ('git-walktree-blob-mode-hook 'eriks/force-emacs-initial-state)
  :general
  ('(git-walktree-mode-map git-walktree-blob-mode-map)
   "c" 'eriks/git-walktree-show-commit)
  ('normal
   :prefix eriks/leader
   "gT" 'git-walktree))

(use-package what-the-commit
  :ensure t
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "w" 'what-the-commit-insert))

(use-package git-gutter-fringe+
  :ensure t
  :diminish git-gutter+-mode
  :custom
  (git-gutter-fr+-side 'right-fringe)
  :config
  (global-git-gutter+-mode 1)
  (eriks/frames-only-use-window-funcs 'git-gutter+-revert-hunk)
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "g" 'git-gutter+-mode
   "j" 'git-gutter+-next-hunk
   "k" 'git-gutter+-previous-hunk
   "r" 'git-gutter+-revert-hunk))

(use-package git-messenger
  :ensure t
  :custom
  (git-messenger:use-magit-popup t)
  (git-messenger:show-detail t)
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "m" 'git-messenger:popup-message))
