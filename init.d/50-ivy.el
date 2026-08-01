(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)

  ;;NOTE: highlight the current line all the way to the window border
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line)

  ;; NOTE: don't sort directories first in `find-file'
  (setf (alist-get 'read-file-name-internal ivy-sort-functions-alist)
        '(ivy-string< ivy-sort-file-function-default))

  (evil-collection-ivy-setup)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (evil-set-initial-state 'ivy-occur-mode 'normal)

  :general-config
  ('ivy-mode-map
   "C-c C-r" 'ivy-resume)
  ('ivy-minibuffer-map
   ;; NOTE: make it more bash-like, this is kill-region normally
   "C-w" 'ivy-backward-kill-word
   ;; TODO: make M-n and M-p search matching input ala `previous-complete-history-element'
   "C-p" 'ivy-previous-history-element
   "C-n" 'ivy-next-history-element
   ;;NOTE: I don't like `ivy-partial-or-done'.
   "TAB" 'ivy-alt-done
   "C-SPC" 'ivy-mark
   "M-SPC" 'ivy-unmark-backward
   ;;NOTE: I don't use hydra
   "C-o" 'ivy-partial
   ;; NOTE: disable the mouse to let my other mouse bindings switch window focus only
   [mouse-1] nil
   [mouse-3] nil)
  ('ivy-occur-mode-map
   [mouse-1] nil)
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
  :custom
  (counsel-preselect-current-file t)
  :config
  (counsel-mode 1)
  (eriks/leader-def 'normal 'counsel-mode-map
    "y" 'counsel-yank-pop)

  ;;NOTE: add smart-case to ripgrep
  (push "--smart-case" (nthcdr (cl-position "%s" counsel-rg-base-command :test 'equal)
                               counsel-rg-base-command))

  (define-advice counsel--git-root (:override () projectile)
    "Make commands like `counsel-rg' use projectile for project root instead
of its default of only looking for git folders."
    (projectile-acquire-root))

  (define-advice counsel-find-file (:after (&rest _args) fix-default-dir)
    "Fix bug when moving a file using `counsel-find-file-move'.

The `default-directory' keeps its old value when moving the file of the
current buffer to a different directory. It should be changed to the
directory of the visited file, that's what `set-visited-file-name' and
`find-file' does. Counsel keeps the old value because it let binds it in
`counsel--find-file-1'."
    (when buffer-file-name
      (setq default-directory (file-name-directory buffer-file-name))))

  :general-config
  ;; NOTE: remaps don't work in minibuffer-local-map and similar for some reason. The
  ;; activation of `counsel-mode' adds a binding there directly too.
  ('evil-command-line-map
   ;; NOTE: C-r is taken by evil
   "M-r" 'counsel-minibuffer-history)
  ('counsel-mode-map
   "M-s" 'counsel-rg
   ;; NOTE: the original is better, and evil-owl covers most cases.
   ;; [remap evil-show-registers] 'counsel-evil-registers
   [remap org-goto] 'counsel-org-goto
   [remap eshell-previous-matching-input] 'counsel-esh-history
   ;; NOTE: I think the normal buffer is better
   ;; [remap flycheck-list-errors] 'counsel-flycheck
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
   ;; NOTE: A space can be used as an regex as [ ]
   "C-s" 'swiper))

(use-package eriks-counsel-evil-marks
  :general
  ([remap evil-show-marks] #'eriks/counsel-evil-marks))

;;NOTE: needs to be loaded last of all ivy packages that touch the transformers
;;https://github.com/Yevgnen/ivy-rich?tab=readme-ov-file#notes
(use-package ivy-rich
  :ensure t
  :config
  ;; TODO: The path is not shown for buffers NOT in a project
  ;; https://github.com/Yevgnen/ivy-rich/issues/108
  ;;NOTE: remove the buffer size from switch buffer
  (cl-callf2 assq-delete-all
      'ivy-rich-switch-buffer-size
      (plist-get
       (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
       :columns))
  (eriks/counsel-evil-mark-install-ivy-rich)
  (ivy-rich-mode 1))

(use-package ivy-xref
  :ensure t
  :config
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package eriks-counsel-extern)

(use-package eriks-counsel-dired-jump)
