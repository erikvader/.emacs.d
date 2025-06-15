(use-package magit
  ;;TODO: gör ett advice runt `magit-insert-untracked-files' som ser till att den alltid visar alla untracked filer
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

(use-package diff-hl
  :ensure t
  :gfhook
  ;;NOTE: the readme says to include these if magit is pretty new
  ('magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  ('magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1)
  (eriks/frames-only-use-window-funcs 'diff-hl-revert-hunk)
  :custom
  (diff-hl-side 'right)
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "r" 'diff-hl-revert-hunk
   "j" 'diff-hl-next-hunk ;TODO: use show versions instead?
   "k" 'diff-hl-previous-hunk))

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
