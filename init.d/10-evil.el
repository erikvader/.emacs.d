(use-package evil
  :ensure t
  :init
  (defconst eriks/leader "SPC" "My leader key for evil")
  :custom
  (evil-emacs-state-cursor '(hollow))
  (evil-cross-lines t)
  (evil-default-state 'emacs)
  (evil-ex-visual-char-range t)
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)
  (evil-search-module 'evil-search)
  (evil-shift-width 3)
  (evil-want-C-d-scroll nil)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-emacs-state-modes nil)
  (evil-motion-state-modes '(Man-mode help-mode))
  (evil-insert-state-modes nil)
  (evil-normal-state-modes '(prog-mode org-mode conf-mode latex-mode))
  :general
  ('(normal visual)
   "K" 'ignore)
  ('motion
   'help-mode-map
   "<tab>" 'forward-button
   "<backtab>" 'backward-button
   "C-o" 'help-go-back
   "C-i" 'help-go-forward
   "<return>" 'push-button
   "g" 'revert-buffer
   "q" 'quit-window
   "<backspace>" 'evil-ex-nohighlight)
  ('motion
   'Man-mode-map
   "<backspace>" 'evil-ex-nohighlight)
  ('emacs "<escape>" 'evil-exit-emacs-state)
  ('motion
   "SPC" nil)
  (evil-ex-completion-map
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "<up>" 'ignore
   "<left>" 'ignore
   "<down>" 'ignore
   "<right>" 'ignore
   "C-e" 'end-of-line)
  ('normal
   "<backspace>" 'evil-ex-nohighlight)
  ('(normal visual)
   ";" 'repeat
   "-" 'negative-argument)
  ('normal
   :prefix eriks/leader
   "q" 'kmacro-insert-counter
   "Q" 'kmacro-set-counter)
  :config
  (evil-mode 1)
  (defmacro eriks/evil-define-inner-local-textobject (key func)
    "binds key to text object func buffer-locally (mostly for my fork of evil-surround)"
    `(progn
       (define-key evil-visual-state-local-map   (kbd ,(concat "i " key)) ,func)
       (define-key evil-operator-state-local-map (kbd ,(concat "i " key)) ,func)))
  (defmacro eriks/evil-define-outer-local-textobject (key func)
    "binds key to text object func buffer-locally (mostly for my fork of evil-surround)"
    `(progn
       (define-key evil-visual-state-local-map   (kbd ,(concat "a " key)) ,func)
       (define-key evil-operator-state-local-map (kbd ,(concat "a " key)) ,func))))

(use-package golden-ratio-scroll-screen
  :ensure t
  :after evil
  :general
  ('motion
   "C-d" 'golden-ratio-scroll-screen-up
   "C-u" 'golden-ratio-scroll-screen-down)
  ('visual
   "C-d" 'evil-scroll-down
   "C-u" 'evil-scroll-up)
  ("M-u" 'universal-argument)
  (universal-argument-map
   "M-u" 'universal-argument-more
   "C-u" nil))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :custom
  (undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode 1)
  :general
  ('normal
   "U" 'undo-tree-redo)
  ('normal
   :prefix eriks/leader
   "u" 'undo-tree-visualize))

(use-package drag-stuff
  :ensure t
  :after evil
  :general
  ('(normal visual)
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down
   "M-h" 'drag-stuff-left
   "M-l" 'drag-stuff-right))

(use-package evil-args
  :ensure t
  :after evil
  :general
  ('inner
   "a" 'evil-inner-arg)
  ('outer
   "a" 'evil-outer-arg))

(use-package eriks-evil-open-join-line
  :after evil
  :general
  ('normal
   "S" 'eriks/evil-open-line
   "<return>" 'eriks/evil-open-line-below
   "S-<return>" 'eriks/evil-open-line-above
   "M-^" 'eriks/evil-join-no-comment-backward)
  ('(normal visual)
   "gJ" 'eriks/evil-join-no-space
   "J"  'eriks/evil-join-no-comment))

(use-package eriks-evil-random
  :after evil
  :general
  ('normal
   :prefix eriks/leader
   "g8" 'eriks/evil-what-cursor))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :general
  ('normal
   "gc" 'evilnc-comment-operator
   "gC" 'evilnc-copy-and-comment-operator)
  ('inner
   "c" 'evilnc-inner-comment)
  ('outer
   "c" 'evilnc-outer-commenter))

(use-package evil-extra-operator
  ;; has an operator for highlighting too
  :ensure t
  :after evil
  :general
  ('(normal visual)
   "gp" 'evil-operator-clone
   "gr" 'evil-operator-eval))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  :general
  ('visual
   'evil-surround-mode-map
   "s" 'evil-surround-region
   "S" 'evil-Surround-region)
  ('normal
   'evil-surround-mode-map
   "gs" 'evil-surround-edit
   "gS" 'evil-Surround-edit)
  :gfhook
  ('LaTeX-mode-hook (cl-defun latex-evil-surround-hook ()
                      (add-to-list 'evil-surround-pairs-alist '(?$ . ("$" . "$")))
                      (add-to-list 'evil-surround-pairs-alist '(?m . latex-surround-env)))))

(use-package eriks-evil-column-numbers
  :after evil
  :general
  ('visual
   :prefix eriks/leader
   :infix "i"
   "n" 'eriks/evil-column-numbers-insert-numbers
   "z" 'eriks/evil-column-numbers-insert-numbers-zero
   "l" 'eriks/evil-column-numbers-insert-letters
   "i" 'eriks/evil-column-numbers-insert))

(use-package eriks-delete-empty-parens
  :after evil
  :general
  ('normal
   :prefix eriks/leader
   "s" 'eriks/delete-empty-parens)
  ('insert
   "C-s" 'eriks/delete-empty-parens))

(use-package eriks-line-cleanup
  :after evil
  :general
  ('(normal insert)
   "C-a" 'eriks/line-cleanup-dwim))

(use-package evil-exchange
  :ensure t
  :after evil
  :config
  (evil-exchange-install))

(use-package evil-multiedit
  :ensure t
  :after evil
  :custom
  (evil-multiedit-follow-matches t)
  :general
  ('iedit-mode-occurrence-keymap
   "M-n" nil
   "M-p" nil)
  ('evil-multiedit-state-map
   "<return>" 'evil-multiedit-toggle-or-restrict-region)
  ('(normal visual)
   "C-n" 'evil-multiedit-match-and-next
   "C-p" 'evil-multiedit-match-and-prev)
  ('evil-multiedit-state-map
   "M-n" 'evil-multiedit-next
   "M-p" 'evil-multiedit-prev))

(use-package eriks-evil-line-to
  :after evil
  :general
  ('inner
   "l" 'eriks/evil-inside-line-text-object)
  ('outer
   "l" 'eriks/evil-outside-line-text-object))

(use-package eriks-fix-last-shift-mistake
  :after evil
  :general
  ('insert
   "M-c" 'eriks/fix-last-shift-mistake))

(use-package evil-lion
  :ensure t
  :after evil
  :general
  ('normal
   :prefix "g"
   "a" 'evil-lion-left
   "A" 'evil-lion-right))

(use-package evil-latex-textobjects
  :after (:and evil tex-mode)
  :gfhook
  ('LaTeX-mode-hook (cl-defun latex-evil-latex-textobjects-hook ()
                      (eriks/evil-define-inner-local-textobject "$" 'evil-latex-textobjects-inner-dollar)
                      (eriks/evil-define-outer-local-textobject "$" 'evil-latex-textobjects-a-dollar)
                      (eriks/evil-define-inner-local-textobject "\\" 'evil-latex-textobjects-inner-math)
                      (eriks/evil-define-outer-local-textobject "\\" 'evil-latex-textobjects-a-math)
                      (eriks/evil-define-inner-local-textobject "f" 'evil-latex-textobjects-inner-macro)
                      (eriks/evil-define-outer-local-textobject "f" 'evil-latex-textobjects-a-macro)
                      (eriks/evil-define-inner-local-textobject "m" 'evil-latex-textobjects-inner-env)
                      (eriks/evil-define-outer-local-textobject "m" 'evil-latex-textobjects-an-env))))
