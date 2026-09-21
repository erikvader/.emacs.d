(use-package smerge-mode
  :custom
  ;; HACK: magit has its own config that overrides and defaults to this one, so make sure
  ;; this is set before magit is loaded. `magit-diff-refine-ignore-whitespace'.
  (smerge-refine-ignore-whitespace nil)
  ;; NOTE: keeping this setting with its default yields the best refinements, I have
  ;; tried with it off, and it's not that good.
  ;; (smerge-refine-weight-hack t)
  )

(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-tracked-only nil)
  (magit-define-global-key-bindings nil)
  (magit-diff-use-indicator-faces t)
  (magit-diff-specify-hunk-foreground nil)
  (magit-ediff-dwim-show-on-hunks t)
  (evil-collection-magit-use-$-for-end-of-line nil)
  (evil-collection-magit-use-0-for-beginning-of-line nil)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (defun eriks/magit-refresh-with-all-untracked-files ()
    "Shows all untracked files in the current buffer, and do not stop at
untracked directories."
    (interactive)
    (unless (derived-mode-p 'magit-status-mode)
      (user-error "Must be in a magit status buffer"))
    (setq-local magit-status-show-untracked-files 'all)
    (magit-refresh)
    (message "Showing all untracked files"))

  (defun eriks/magit-toggle-whitespace ()
    "Toggle whether whitespace should be ignored.

Diff settings is normally tweaked with `magit-diff-refresh', this is
just a shortcut for the status buffer."
    (interactive)
    (unless (derived-mode-p 'magit-status-mode)
      (user-error "Must be in a magit status buffer"))
    (let ((flag "--ignore-space-change"))
      (pcase-let ((`(,args ,_) (magit-diff-arguments 'magit-status-mode)))
        (setq magit-buffer-diff-args
              (if (member flag args)
                  (remove flag args)
                (cons flag args))))
      (magit-refresh)
      (message "Ignoring whitespace: %s" (if (member flag magit-buffer-diff-args) "yes" "no"))))

  (eriks/leader-def 'normal
    :infix "g"
    "s" 'magit-status
    "h" 'magit-status-here
    "d" 'magit-diff-buffer-file
    "f" 'magit-file-dispatch
    "g" 'magit-dispatch)

  (eriks/leader-def 'normal 'magit-status-mode-map
    :infix "g"
    "w" 'eriks/magit-toggle-whitespace
    "u" 'eriks/magit-refresh-with-all-untracked-files)

  (progn
    (defvar-local eriks/magit-faces-modified nil)

    ;; https://github.com/magit/magit/issues/2942#issuecomment-4069825556
    (defun eriks/magit-diff-fontify-with-diff-mode ()
      "Alternate way to add syntax to magit diffs.

The official way with (magit-diff-fontify-hunk 'all) is too wasteful, it
opens each file and runs all of their hooks. I don't know why it does
that, because that version and this one eventually calls
`diff-syntax-fontify-props', which uses `delay-mode-hooks', so it must
be something on magit's side that runs the mode hooks.

This methods fontifies the diff text that is already there, which is
less accurate, but works well enough imho. I think magit tries much
harder to get the whole file as context, it at least sounds like it on
the issue discussing this feature."
      ;; NOTE: make it easier to read dark and dim colors against bright refinement
      ;; backgrounds. This is affecting the whole magit diff buffer, but that is probably
      ;; fine since this is targeting font lock faces that magit is probably not using.
      (unless eriks/magit-faces-modified
        (setq-local eriks/magit-faces-modified t)
        (let ((bright-shadow (color-lighten-name (face-foreground 'shadow nil 'default) 20)))
          (dolist (dark '(font-lock-comment-face font-lock-doc-face))
            (face-remap-add-relative dark :foreground bright-shadow))))

      ;; HACK: the major mode is activated inside a delay-mode-hooks inside the function
      ;; `diff-syntax-fontify-props', which is the function that converts all text
      ;; properties into overlays. My own font locks are enabled through these now delayed
      ;; hooks, so they need to be forcefully enabled for the function to find them and
      ;; return them in the correct display order.
      (cl-letf* ((org-fun (symbol-function 'set-auto-mode))
                 ((symbol-function 'set-auto-mode) (lambda (&rest args2)
                                                     (apply org-fun args2)
                                                     (eriks/add-marker-font-locks)
                                                     (eriks/add-tab-font-lock))))
        (save-excursion
          (let ((min (point-min))
                (max (point-max)))
            (save-restriction
              (widen)
              (setq-local buffer-read-only nil)
              (setq-local diff-font-lock-syntax 'hunk-also)
              (goto-char min)
              (diff--font-lock-syntax max))))))

    (add-hook 'magit-diff-wash-diffs-hook 'eriks/magit-diff-fontify-with-diff-mode))

  (evil-collection-magit-setup)
  (evil-set-initial-state 'git-commit-mode 'insert)
  :general-config
  ;; NOTE: change these bindings in the same maps as evil-collection does
  ('(magit-file-section-map magit-hunk-section-map)
   [remap magit-diff-visit-worktree-file] 'magit-diff-visit-worktree-file-other-window
   [remap magit-diff-visit-file] 'magit-diff-visit-file-other-window)
  ('(magit-revision-mode-map magit-status-mode-map)
   ;; NOTE: let my leader through
   "SPC" nil)
  ('(magit-status-mode-map magit-diff-mode-map)
   ;; NOTE: matches the binding in `evil-collection-diff-mode-setup'. This is the command
   ;; that `magit-diff-toggle-refine-hunk' eventually calls.
   "*" 'diff-refine-hunk)
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
  (vc-display-status nil)
  (vc-handled-backends '(Git))
  (vc-follow-symlinks nil))

(use-package git-modes
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("/git/config\\..+\\'" . gitconfig-mode)))

(use-package ediff
  :config
  (evil-collection-ediff-setup)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :gfhook
  ('ediff-startup-hook #'ediff-next-difference))

(use-package diff-mode
  :custom
  (diff-font-lock-syntax 'hunk-also)
  (diff-refine nil)
  :config
  (evil-collection-diff-mode-setup)
  ;; TODO: it would be nice if this could toggle all hunks on a file in magit
  ;; TODO: it would be cool if a hunk automatically got refined only if the number of
  ;; deleted lines is equal to the number of added lines. This would hopefully remove most
  ;; "this word got replaced with 10 lines", which is the most useless applifaction of
  ;; refinement.
  (define-advice diff-refine-hunk (:around (org) toggle)
    "Makes this function toggle the refinement in the current hunk."
    (cl-destructuring-bind (beg end) (diff-bounds-of-hunk)
      (if (cl-some (lambda (ovl) (eq 'fine (overlay-get ovl 'diff-mode)))
                   (overlays-in beg end))
          (remove-overlays beg end 'diff-mode 'fine)
        (save-excursion
          (diff--refine-hunk beg end)))))

  ;; BUG: the + and - are included in the refinement as code changes.
  ;; This function is run on each hunk before being refined, and this is how it looked in
  ;; 2007. I don't know if this actually fixed the problem completely, a simple case got
  ;; fixed though, but the pluses and minuses are are still sometimes highlighed and
  ;; sometimes not. Not sure if this is even fixable.
  ;; (define-advice diff-refine-preproc (:override () fix-refinement)
  ;;   (while (re-search-forward "^." nil t)
  ;;     (replace-match " ")))

  :general-config
  ('diff-mode-map
   ;; NOTE: let my `ace-window' through
   "M-o" nil))
