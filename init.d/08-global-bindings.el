(use-package eriks-defkey-repeat)

(use-package which-key
  ;; Enable with `which-key-mode'
  :ensure t
  :diminish)

(use-package repeat
  :config
  (repeat-mode 1))

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")

;; bind some commands on global bindings I otherwise don't use
(general-def
  :prefix "C-x"
  "C-u" 'universal-argument
  "C-M-e" 'eriks/eval-replace-last-sexp)

(general-def
  :prefix "C-h"
  "w" 'eriks/what-face
  "c" 'describe-char
  "F" 'describe-face
  "M" 'describe-keymap)

(general-def
  "<next>" 'scroll-other-window
  "<prior>" 'scroll-other-window-down
  "C-u" 'kill-whole-line
  "M-y" 'clipboard-yank)
