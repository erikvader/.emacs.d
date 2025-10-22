(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)

  ;;NOTE: highlight the current line all the way to the window border
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line)

  (evil-collection-ivy-setup)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (evil-set-initial-state 'ivy-occur-mode 'normal)

  (eriks/leader-def 'normal
    "r" 'ivy-resume)

  :general-config
  ('(ivy-minibuffer-map minibuffer-local-map)
   "C-r" 'counsel-minibuffer-history
   "<escape>" 'minibuffer-keyboard-quit
   ;;TODO: this won't use evil kill word, but ivy kill word. Possible to merge them?
   "C-w" 'backward-kill-word)
  ('ivy-minibuffer-map
   ;;NOTE: I don't like `ivy-partial-or-done'.
   "TAB" 'ivy-partial
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

;; NOTE: `ivy-define-key' removes the commands in defines from `counsel-M-x'.
(use-package counsel
  :ensure t
  :diminish
  :config
  (counsel-mode 1)
  (eriks/leader-def 'normal
    "y" 'counsel-yank-pop)

  ;;NOTE: add smart-case to ripgrep
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
   ;; NOTE: the original is better, and evil-owl covers most cases.
   ;; [remap evil-show-registers] 'counsel-evil-registers
   ;; NOTE: this one simply doesn't work with global markers, it expects all of them to be
   ;; in the same buffer.
   ;; https://github.com/abo-abo/swiper/issues/2707#issuecomment-748765715
   ;; https://github.com/abo-abo/swiper/pull/2247
   ;; TODO: make my own version that works?
   ;; [remap evil-show-marks] 'counsel-evil-marks
   [remap org-goto] 'counsel-org-goto
   [remap eshell-previous-matching-input] 'counsel-esh-history
   [remap flycheck-list-errors] 'counsel-flycheck
   [remap comint-history-isearch-backward-regexp] 'counsel-shell-history
   [remap dired] 'counsel-dired
   [remap describe-face] 'counsel-faces
   [remap describe-bindings] nil
   ;;NOTE: this counsel variant doesn't work with `other-window-prefix'. It otherwise only
   ;;seems to add actions which I don't use.
   ;; [remap recentf-open-files] 'counsel-recentf
   [remap swiper] 'counsel-grep-or-swiper))

(use-package eriks-counsel-projectile-switch-project
  :general-config
  ('counsel-mode-map
   [remap projectile-switch-project] #'eriks/counsel-projectile-switch-project))

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
  ;;NOTE: remove the buffer size from switch buffer
  (cl-callf2 assq-delete-all
      'ivy-rich-switch-buffer-size
      (plist-get
       (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
       :columns))
  (ivy-rich-mode 1))

(use-package ivy-xref
  :ensure t
  :config
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
