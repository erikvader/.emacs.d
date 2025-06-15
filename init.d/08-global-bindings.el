;; repeat-mode is not supported in general, ever: https://github.com/noctuid/general.el/issues/552
(cl-defmacro eriks/bind-key-repeat (map-name &rest general-keydefs)
  "The arguments are the same as those given to `general-define-key' and
does the same things, except that the last key in each binding can be
pressed again to repeat that command. This utilizes `repeat-mode'."
  (declare (indent defun))
  (cl-destructuring-bind (binds kwargs) (general--remove-keyword-args general-keydefs)
    `(progn
       ;;TODO: not sure if these support the same kinds of arguments, specifically the key
       ;;strings. General support `[remap blah]', but does defvar-keymap?
       (general-define-key ,@kwargs ,@binds)
       ;;TODO: support the options to :repeat. Possibly as keyword arguments to each bind?
       (defvar-keymap ,map-name :repeat t ,@binds))))

(use-package which-key
  ;; Enable with `which-key-mode'
  :ensure t
  :diminish)

(use-package repeat
  :config
  (repeat-mode 1))

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")

;; I don't use these kmacro bindings anyways
(general-def
  "C-x C-k" 'kill-current-buffer
  "C-x e" 'pp-macroexpand-last-sexp)

;; Move the universal argument
(general-def 'universal-argument-map
  "C-t" 'universal-argument-more
  "C-u" nil)

(general-def
  "C-t" 'universal-argument
  "C-u" nil)

(defmacro eriks/universal-argument (&rest keymaps)
  `(general-def ,@keymaps "C-t" 'universal-argument))

;; Next and prev buffers
(general-def 'buffer-navigation-repeat-map
  "l" 'next-buffer
  "h" 'previous-buffer)

(general-def
  :prefix "C-x"
  "l" 'next-buffer
  "h" 'previous-buffer)

;; Make emacs behave more like the terminal
(general-def
  "C-w" 'backward-kill-word)
