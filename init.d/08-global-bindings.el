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
  "C-k" 'quit-window
  "C-o" 'display-buffer
  "m" 'pp-macroexpand-last-sexp
  "M-e" 'eriks/eval-replace
  "C-u" 'universal-argument)

(general-def
  :prefix "C-h"
  "c" 'describe-char)

(general-def
  "<next>" 'scroll-other-window
  "<prior>" 'scroll-other-window-down
  "C-h F" 'describe-face
  "C-u" 'kill-whole-line
  "M-y" 'clipboard-yank)
