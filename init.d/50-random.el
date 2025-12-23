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
  :config
  (evil-collection-ibuffer-setup)
  :custom
  (ibuffer-saved-filter-groups '(("eriks"
                                  ("Files" (and (visiting-file . t)
                                                (predicate . (not buffer-read-only))))
                                  ("Dired" (mode . dired-mode))
                                  ("Man" (mode . Man-mode))
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
    :infix "o"
    "t" 'eriks/spawn-external-terminal))

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
  ;; TODO: evil-collection
  :general-config
  ('pdf-view-mode-map
   [remap swiper] 'isearch-forward
   "C-s" 'isearch-forward
   "/" 'isearch-forward
   "O" 'pdf-occur
   "r" 'image-rotate
   "q" 'quit-window
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

;; TODO: use-package nov for epubs?

;; TODO: evil collection
(use-package wgrep
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish
  :custom
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)
  :config
  ;; NOTE: it doesn't disable itself in ivy. Undo in counsel find file doesn't work
  (add-to-list 'undo-tree-incompatible-major-modes 'minibuffer-mode)
  (global-undo-tree-mode 1)
  (eriks/leader-def 'normal
    "u" 'undo-tree-visualize))

(use-package scroll-lock
  :gfhook #'reset-scroll-margin
  :general-config
  ('scroll-lock-mode-map
   [remap evil-next-line] 'scroll-lock-next-line
   [remap evil-previous-line] 'scroll-lock-previous-line
   [remap evil-forward-paragraph] 'scroll-lock-forward-paragraph
   [remap evil-backward-paragraph] 'scroll-lock-backward-paragraph))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package vline
  :ensure t
  :general-config
  (eriks/leader-def 'normal
    "v" 'vline-mode))

(use-package rainbow-mode
  :ensure t)

;; TODO: how to change the major mode to get syntax highlighting after this has opened?
(use-package view
  :config
  (evil-collection-view-setup))

(use-package simple
  :config
  (evil-collection-simple-setup)
  (evil-set-initial-state 'special-mode 'normal)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (when-let ((msgs (get-buffer messages-buffer-name)))
    (evil-change-to-initial-state msgs))

  (add-to-list 'evil-buffer-regexps `(,(eriks/regexp-quote-all shell-command-buffer-name) . normal)))

(use-package debug
  :config
  (evil-collection-debug-setup)
  (evil-collection-define-key 'normal 'debugger-mode-map
    "q" 'debugger-quit))

(use-package compile
  :custom
  ;; TODO: doesn't seem to scroll all the way to the bottom. Is the scroll margin
  ;; interfering? Is there a scroll maximum as in eshell?
  (compilation-scroll-output t)
  :gfhook
  ('compilation-mode-hook #'reset-scroll-margin)
  :config
  (evil-collection-compile-setup)
  (eriks/leader-def 'normal
    "c" 'compile))

(use-package info
  :config
  ;;TODO: move these bindings to the standalone info program?
  (evil-collection-info-setup))

(use-package pp
  :general-config
  ('global
   :prefix "C-x"
   "m" 'pp-macroexpand-last-sexp
   "M-e" 'pp-eval-last-sexp)
  ('global
   [remap eval-expression] 'pp-eval-expression)
  :config
  (define-advice pp-display-expression (:override (expression out-buffer-name &optional lisp) behave)
    "Make this function behave well with the rest of emacs.

The original function overrides the behavior of
`with-output-to-temp-buffer' by switching back to the previous window
and ignores the rules in `display-buffer-alist', this advice removes
this custom behavior, but is otherwise a copy of it.

While I'm at it, I also removed the possibility to show the result as
`message', a buffer is always shown now, unless the result is nil.

I also added a buffer local binding to quit the window."
    (if expression
        (let* ((lexical lexical-binding))
          (with-output-to-temp-buffer out-buffer-name
            (if lisp
                (with-current-buffer standard-output
                  (pp-emacs-lisp-code expression))
              (pp expression))
            (with-current-buffer standard-output
              (emacs-lisp-mode)
              (eriks/evil-setup-local-quit)
              (setq lexical-binding lexical)
              (setq-local font-lock-verbose nil))))
      (message "%s" expression))))

(use-package calendar
  :custom
  (calendar-week-start-day 1)
  (setopt calendar-intermonth-text
          '(propertize
            (format "%2d"
                    (car
                     (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian (list month day year)))))
            'font-lock-face 'font-lock-function-name-face))
  :config
  (evil-collection-calendar-setup)
  (eriks/leader-def 'normal
    "C" 'calendar)
  :gfhook
  ('calendar-today-visible-hook 'calendar-mark-today))

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  (bookmark-fringe-mark nil)
  :config
  (eriks/leader-def 'normal
    "b" 'bookmark-jump))

;; TODO: create evil motions? Can be implemented by temporarily rebinding the
;; forward-evil-word thing for forward-subword?
(use-package subword
  :config
  (defalias 'forward-subword 'subword-forward "To be compatible with thingatpt.el")
  (evil-define-text-object evil-inner-subword (count &optional beg end _type)
    "Select inner subword"
    ;;NOTE: an outer variant didn't work as expected, but it doesn't really matter
    ;;anyways. The only time it makes a difference, i.e. when there is whitespace on both
    ;;sides, a normal `evil-a-word' will do the same thing.
    (evil-select-inner-object 'subword beg end type count))
  :general-config
  ('inner
   "u" 'evil-inner-subword))
