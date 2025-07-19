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
  "M-e" 'eriks/eval-replace
  "C-u" 'universal-argument)

;; to be able to jump back to previous buffer, mimics the binding from evil
;;TODO: ??
;;TODO: bind the evil jump list commands here instead?
;;TODO: just `previous-buffer'?
;;TODO: `mode-line-other-buffer'
(general-def
  "C-o" 'pop-global-mark)

(general-def
  "<next>" 'scroll-other-window
  "<prior>" 'scroll-other-window-down
  "C-h F" 'describe-face)
