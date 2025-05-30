;; Not supported in general, ever: https://github.com/noctuid/general.el/issues/552
(cl-defmacro eriks/bind-key-repeat (map-name &rest general-keydefs)
  "The arguments are the same as those given to `general-define-key' and
does the same things, except that the last key in each binding can be
pressed again to repeat that command. This utilizes `repeat-mode'."
  (declare (indent defun))
  (cl-destructuring-bind (binds kwargs) (general--remove-keyword-args general-keydefs)
    `(progn
       (general-define-key ,@kwargs ,@binds)
       (defvar-keymap ,map-name :repeat t ,@binds))))

(use-package which-key
  :disabled
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")

;; I don't use these kmacro bindings anyways
(general-def
  "C-x C-k" 'kill-current-buffer
  "C-x e" 'pp-macroexpand-last-sexp)

(use-package repeat
  :config
  (repeat-mode 1))
