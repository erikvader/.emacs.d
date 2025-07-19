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

;; bind some commands in C-x that replaces bindings i otherwise don't use
(general-def
  :prefix "C-x"
  "C-k" 'kill-current-buffer
  "m" 'pp-macroexpand-last-sexp
  "M-e" 'eriks/eval-replace)

;; Make emacs behave more like the terminal
(general-def
  "C-w" 'backward-kill-word)
