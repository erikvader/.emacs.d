(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)

  ;;NOTE: highlight the current line all the way to the window border
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line)

  ;;TODO: Emacs doesn't change focus to the selected frame with
  ;;`ivy-occur-press-and-switch' for some reason. The function `select-frame' says it
  ;;should. See also `select-frame-set-input-focus'

  :general-config
  ('global
   :prefix "C-c"
   "C-r" 'ivy-resume)
  ('(ivy-minibuffer-map minibuffer-local-map)
   "<escape>" 'abort-minibuffers
   ;;TODO: this won't use evil kill word, but ivy kill word. Possible to merge them?
   "C-w" 'backward-kill-word)
  ('ivy-minibuffer-map
   "C-SPC" 'ivy-mark
   "M-SPC" 'ivy-unmark-backward
   ;;NOTE: I don't use hydra
   [remap hydra-ivy/body] 'ivy-dispatching-done)
  :custom
  (ivy-count-format "%d/%d ")
  (ivy-on-del-error-function 'ignore)
  (ivy-height 20)
  (ivy-extra-directories nil))

(use-package ivy-avy
  :ensure t)

(use-package counsel
  :ensure t
  :diminish
  :config
  (counsel-mode 1)
  (eriks/leader-def 'normal
    "y" 'counsel-yank-pop)

  (push "--smart-case" (nthcdr (cl-position "%s" counsel-rg-base-command :test 'equal)
                               counsel-rg-base-command))
  (define-advice counsel--git-root (:override () projectile)
    "Make commands like `counsel-rg' use projectile for project root instead
of its default of only looking for git folders."
    (projectile-acquire-root))

  :general-config
  ('(counsel-ag-map counsel-git-grep-map)
   :prefix "C-c"
   "C-d" 'counsel-cd)
  ('counsel-mode-map
   "M-s" 'counsel-rg
   [remap describe-bindings] nil
   [remap recentf-open-files] 'counsel-recentf
   [remap swiper] 'counsel-grep-or-swiper))

(use-package eriks-counsel-projectile-switch-project
  :general-config
  ('counsel-mode-map
   [remap projectile-switch-project] #'eriks/counsel-projectile-switch-project))

;;TODO: ivy-regex-fuzzy with flx?
(use-package swiper
  :ensure t
  :general-config
  ('ivy-mode-map
   "C-s" 'swiper))

;;NOTE: needs to be loaded last of all ivy packages that touch the transformers
;;https://github.com/Yevgnen/ivy-rich?tab=readme-ov-file#notes
(use-package ivy-rich
  :ensure t
  :config
  (cl-callf2 assq-delete-all
      'ivy-rich-switch-buffer-size
      (plist-get
       (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
       :columns))
  (ivy-rich-mode 1))
