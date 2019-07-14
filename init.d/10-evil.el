(use-package evil
  :ensure t
  :custom
  (evil-emacs-state-modes nil)
  (evil-motion-state-modes nil)
  (evil-insert-state-modes nil)
  (evil-normal-state-modes '(prog-mode))
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
  ('emacs [escape] 'evil-normal-state)
  (evil-ex-completion-map
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "<up>" 'noop
   "<left>" 'noop
   "<down>" 'noop
   "<right>" 'noop)
  ('normal
   "<backspace>" 'evil-ex-nohighlight)
  :config
  (evil-mode 1)
  (defconst eriks/leader "SPC" "My leader key for evil"))

(use-package golden-ratio-scroll-screen
  :ensure t
  :after evil
  :general
  ('normal
   "C-d" 'golden-ratio-scroll-screen-up
   "C-u" 'golden-ratio-scroll-screen-down)
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
