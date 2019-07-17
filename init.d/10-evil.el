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
  :general
  ('emacs "<escape>" 'evil-exit-emacs-state)
  (evil-ex-completion-map
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "<up>" 'ignore
   "<left>" 'ignore
   "<down>" 'ignore
   "<right>" 'ignore)
  ('normal
   "<backspace>" 'evil-ex-nohighlight
   ";" 'repeat)
  ('normal
   :prefix eriks/leader
   "q" 'kmacro-insert-counter
   "Q" 'kmacro-set-counter)
  :config
  (evil-mode 1)
  (setq-default evil-emacs-state-modes nil)
  (setq-default evil-motion-state-modes nil)
  (setq-default evil-insert-state-modes nil)
  (setq-default evil-normal-state-modes '(prog-mode))
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'org-mode 'normal)
  (evil-set-initial-state 'Man-mode 'motion)
  (evil-set-initial-state 'conf-mode 'normal))

(use-package golden-ratio-scroll-screen
  :ensure t
  :after evil
  :general
  ('normal
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
   "S-<return>" 'eriks/evil-open-line-above)
  ('(normal visual)
   :prefix eriks/leader
   "J" 'eriks/evil-join-no-space)
  ('(normal visual)
   "J" 'eriks/evil-join-no-comment)
  ('(normal visual)
   "K" 'ignore))

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
   "gS" 'evil-Surround-edit))

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
