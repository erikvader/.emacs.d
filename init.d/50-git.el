(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (defun eriks/magit-refresh-with-all-untracked-files ()
    "Shows all untracked files in the current buffer, and do not stop at
untracked directories."
    (interactive)
    (setq-local magit-status-show-untracked-files 'all)
    (magit-refresh)
    (message "Showing all untracked files"))

  (eriks/leader-def 'normal
    :infix "g"
    "s" 'magit-status
    "b" 'magit-blame
    "f" 'magit-find-file)

  (add-to-list 'evil-buffer-regexps
               '("^COMMIT_EDITMSG$" . insert))
  :gfhook ('magit-blame-mode-hook #'evil-emacs-state)
  :general-config
  ('magit-status-mode-map
   :prefix "C-c"
   "a" 'eriks/magit-refresh-with-all-untracked-files))

(use-package git-timemachine
  :ensure t
  :config
  (add-to-list 'evil-buffer-regexps
               '("^timemachine:" . emacs))
  (eriks/leader-def 'normal
    "gt" 'git-timemachine))

(use-package what-the-commit
  :ensure t
  :config
  (eriks/leader-def 'normal
    :infix "g"
    "w" 'what-the-commit-insert))

(use-package diff-hl
  :ensure t
  :gfhook
  ;;NOTE: the readme says to include these if magit is pretty new
  ('magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  ('magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ('dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode 1)
  (evil-collection-diff-hl-setup)
  (eriks/leader-def 'normal
    :infix "g"
    "r" 'diff-hl-revert-hunk
    "j" 'diff-hl-next-hunk
    "k" 'diff-hl-previous-hunk
    "o" 'diff-hl-show-hunk)
  :custom
  ;; TODO: disable the map somehow `diff-hl-command-prefix'
  (diff-hl-side 'right)
  (diff-hl-show-hunk-inline-popup-smart-lines nil)
  (diff-hl-show-hunk-inline-popup-hide-hunk t))

(use-package git-messenger
  :ensure t
  :custom
  (git-messenger:use-magit-popup t)
  (git-messenger:show-detail t)
  :config
  (eriks/leader-def 'normal
    :infix "g"
    "m" 'git-messenger:popup-message))
