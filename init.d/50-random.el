(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/theme nil)
  :config
  (sml/setup)
  (unless column-number-indicator-zero-based
    (setopt sml/col-number-format (cl-substitute ?C ?c sml/col-number-format)))
  (setopt sml/show-encoding (cl-substitute ?Z ?z sml/show-encoding))
  (setopt mode-line-percent-position '(-3 "%o")))

(use-package projectile
  :ensure t
  :diminish
  :config
  (put 'projectile-project-root 'safe-local-variable #'stringp)
  (projectile-mode 1)
  :general-config
  ('projectile-mode-map
   :prefix "C-c"
   "p" 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-expand-only-for-last-commands '(self-insert-command org-self-insert-command))
  (yas-also-indent-empty-lines t)
  :gfhook
  ('snippet-mode-hook #'disable-require-final-newline)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package ediff
  :config
  (evil-collection-ediff-setup)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ibuffer
  :custom
  (ibuffer-saved-filter-groups '(("eriks"
                                  ("Files" (and (visiting-file . t)
                                                (predicate . (not buffer-read-only))))
                                  ("Dired" (or (mode . dired-mode)
                                               (mode . dirvish-special-preview-mode)
                                               (mode . dirvish-directory-view-mode)
                                               (mode . dirvish-better-directory-view-mode)
                                               (name . "PREVIEW :: .*")))
                                  ("Starred" (starred-name . t)))))
  :gfhook
  ('ibuffer-mode-hook (cl-defun eriks/ibuffer-switch-to-my-filter-group ()
                        (ibuffer-switch-to-saved-filter-groups "eriks")))
  :general-config
  ("C-x C-b" 'ibuffer))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :config
  (defun eriks/dumb-jump-projectile-project-root (filepath)
    "Use projectile to find the root to search in, using
`default-directory' if not in a project. NOTE: Does not consider
.dumbjump or .dumbjumpignore files anymore."
    (or (projectile-project-root filepath)
        default-directory))
  (advice-add 'dumb-jump-get-project-root :override #'eriks/dumb-jump-projectile-project-root)
  :gfhook
  ('xref-backend-functions 'dumb-jump-xref-activate))

(use-package eriks-random-stuff
  :config
  (eriks/leader-def 'normal
    "o" 'eriks/spawn-external-terminal)
  :general-config
  ('dired-mode-map
   "O" 'eriks/spawn-external-terminal))

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-page)
  :gfhook
  ('pdf-view-mode-hook (cl-defun eriks/pdf-view-no-evil-cursor ()
                         "A bug where the cursor creates a thin border around the pages.
Also that `evil-set-initial-state' does not always work"
                         (evil-emacs-state)
                         (setq-local evil-emacs-state-cursor '(nil))))

  :config
  ;; This is the part in `pdf-loader-install' that gets run when pdf-tools is not loaded.
  (require 'pdf-loader)
  (pdf-loader--install #'pdf-loader--load)

  (defun eriks/pdf-goto-page (arg)
    "Goto first page if no prefix argument, otherwise goto page ARG."
    (interactive "p")
    (pdf-view-goto-page arg))

  (defun eriks/pdf-view-kill-ring-save-clipboard ()
    "Runs `pdf-view-kill-ring-save' but save to system clipboard."
    (interactive)
    (let ((select-enable-clipboard t))
      (pdf-view-kill-ring-save)))

  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (eriks/frames-only-use-window-funcs 'pdf-outline)
  :general-config
  ('pdf-view-mode-map
   [remap swiper] 'isearch-forward
   "C-s" 'isearch-forward
   "/" 'isearch-forward
   "O" 'pdf-occur
   "r" 'image-rotate
   "q" 'quit-window-kill
   "w" 'pdf-view-fit-page-to-window
   "W" 'pdf-view-fit-width-to-window
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page
   "h" 'image-backward-hscroll
   "l" 'image-forward-hscroll
   "." 'pdf-view-next-page-command
   "," 'pdf-view-previous-page-command
   "H" 'image-bob
   "L" 'image-eob
   "y" 'eriks/pdf-view-kill-ring-save-clipboard
   "g" nil
   "gr" 'revert-buffer
   "gg" 'eriks/pdf-goto-page
   "G" 'pdf-view-last-page
   "n" 'pdf-history-forward
   "p" 'pdf-history-backward
   "C-o" 'pdf-history-backward
   "C-i" 'pdf-history-forward)
  ('pdf-history-minor-mode-map
   "r" nil)
  ('pdf-links-minor-mode-map
   "f" 'pdf-links-action-perform
   "F" 'pdf-links-isearch-link)
  ('isearch-mode-map
   "<escape>" 'isearch-abort)
  ('pdf-outline-buffer-mode-map
   "RET" 'pdf-outline-follow-link-and-quit))

(use-package wgrep
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish
  :custom
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)
  :config
  (eriks/frames-only-use-window-funcs 'undo-tree-visualize) ;; undo-tree moves focus between frames on every action
  (global-undo-tree-mode 1)
  (eriks/leader-def 'normal
    "u" 'undo-tree-visualize))

(use-package scroll-lock
  :general-config
  ('scroll-lock-mode-map
   [remap evil-next-line] 'scroll-lock-next-line
   [remap evil-previous-line] 'scroll-lock-previous-line
   [remap evil-forward-paragraph] 'scroll-lock-forward-paragraph
   [remap evil-backward-paragraph] 'scroll-lock-backward-paragraph))

(use-package iedit
  :ensure t
  :config
  (evil-define-operator eriks/evil-iedit-restrict (beg end type)
    "Restrict iedit by using an evil motion."
    :move-point nil
    (interactive "<R>")
    (unless iedit-mode
      (user-error "not in iedit-mode"))
    (iedit-restrict-region beg end))

  (defun eriks/iedit-reactivate-normal-state (&rest _rest)
    "Workaround for keybinds in normal state when iedit is
active. The keybinds don't realize they should be active unless
normal state is reactivated."
    (evil-normal-state))
  (advice-add 'iedit-mode :after #'eriks/iedit-reactivate-normal-state)
  :general-config
  ('normal
   "gr" 'iedit-mode)
  ('normal
   'iedit-mode-keymap
   "n" 'iedit-next-occurrence
   "N" 'iedit-prev-occurrence
   "gr" 'eriks/evil-iedit-restrict
   "gf" 'iedit-restrict-function
   "gl" 'iedit-restrict-current-line
   "<escape>" 'iedit-mode)
  ('normal
   'iedit-mode-occurrence-keymap
   "gt" 'iedit-toggle-selection
   "D" 'iedit-delete-occurrences))

(use-package flyspell
  :general-config
  ('flyspell-mode-map
   "C-;" nil))

(use-package help-mode
  :gfhook 'scroll-lock-mode
  :general-config
  ('motion
   'help-mode-map
   "<tab>" 'forward-button
   "<backtab>" 'backward-button
   "C-o" 'help-go-back
   "C-i" 'help-go-forward
   "<return>" 'push-button
   "r" 'revert-buffer
   "q" 'quit-window
   "<backspace>" 'evil-ex-nohighlight))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package vline
  :ensure t
  :general-config
  (eriks/leader-def 'normal
    "v" 'vline-mode))
