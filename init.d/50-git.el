(use-package magit
  :ensure t
  :custom
  (magit-define-global-key-bindings nil)
  (magit-diff-refine-hunk nil)
  (evil-collection-magit-use-$-for-end-of-line nil)
  (evil-collection-magit-use-0-for-beginning-of-line nil)
  (evil-collection-magit-use-z-for-folds t)
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
    "f" 'magit-file-dispatch
    "g" 'magit-dispatch)

  (eriks/leader-def 'normal 'magit-status-mode-map
    "u" 'eriks/magit-refresh-with-all-untracked-files)

  (evil-collection-magit-setup)
  (evil-set-initial-state 'git-commit-mode 'insert)
  :general-config
  ('(magit-revision-mode-map magit-status-mode-map)
   ;; NOTE: let my leader through
   "SPC" nil)
  ('magit-status-mode-map
   ;; NOTE: match the binding in `evil-collection-diff-mode-setup'
   "g*" 'diff-refine-hunk)
  ('normal
   'magit-mode-map
   ;; NOTE: evil-collection shadows these motion state bindings on purpose, it seems, they
   ;; at least aren't there. So this re-adds all of those z bindings from evil-maps.el.
   "z^" 'evil-scroll-top-line-to-bottom
   "z+" 'evil-scroll-bottom-line-to-top
   "zt" 'evil-scroll-line-to-top
   "z RET" 'evil-scroll-line-to-top-first-non-blank
   "zz" 'evil-scroll-line-to-center
   "z." 'evil-scroll-line-to-center-first-non-blank
   "zb" 'evil-scroll-line-to-bottom
   "z-" 'evil-scroll-line-to-bottom-first-non-blank
   "zl" 'evil-scroll-column-right
   "z <right>" 'evil-scroll-column-right
   "zh" 'evil-scroll-column-left
   "z <left>" 'evil-scroll-column-left
   "ze" 'evil-scroll-end-column
   "zs" 'evil-scroll-start-column
   "zH" 'evil-scroll-left
   "z S-<right>" 'evil-scroll-left
   "zL" 'evil-scroll-right
   "z S-<right>" 'evil-scroll-right))

(use-package git-timemachine
  :ensure t
  :config
  (add-to-list 'evil-buffer-regexps '("^timemachine:" . normal))
  (evil-collection-git-timemachine-setup)
  (eriks/leader-def 'normal
    :infix "g"
    "t" 'git-timemachine))

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
  ;; NOTE: this mode forcefully binds a map to `diff-hl-command-prefix', so this is a
  ;; workaround to disable that binding.
  (general-define-key :keymaps 'diff-hl-mode-map diff-hl-command-prefix nil)
  :custom
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

(use-package vc
  :custom
  (vc-follow-symlinks nil))
