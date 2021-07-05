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
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  :general
  ('(normal visual)
   "M-u" 'universal-argument)
  (universal-argument-map
   "M-u" 'universal-argument-more
   "C-u" nil)
  ('emacs "<escape>" 'evil-exit-emacs-state)
  ('motion
   "SPC" nil
   "M-e" 'evil-forward-sentence-begin
   "M-a" 'evil-backward-sentence-begin)
  (evil-ex-completion-map
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "<up>" 'ignore
   "<left>" 'ignore
   "<down>" 'ignore
   "<right>" 'ignore
   "C-e" 'end-of-line
   "C-r" 'eriks/paste-from-register)
  ('normal
   "<backspace>" 'evil-ex-nohighlight
   "C-@" "@@"
   "C-x M-e" 'eriks/eval-replace
   "U" 'evil-redo)
  ('(normal visual)
   ";" 'repeat
   "-" 'negative-argument)
  ('normal
   :prefix eriks/leader
   "C-o" 'browse-url-at-point
   "q" 'kmacro-insert-counter
   "Q" 'kmacro-set-counter
   "&" 'evil-ex-repeat-substitute-with-flags)
  :config
  (evil-mode 1)
  ;; Doesn't work to set these in :custom, They overwrite later calls
  ;; to `evil-set-initial-state' for some reason.
  (setq evil-emacs-state-modes nil
        evil-motion-state-modes '(help-mode)
        evil-insert-state-modes nil
        evil-normal-state-modes '(prog-mode conf-mode))
  (defun eriks/force-emacs-initial-state ()
    "A handy way to set initial state for a minor mode. The buffer's
normal initial state is ignored."
    (evil-emacs-state)
    (setq-local evil-buffer-regexps nil)
    (setq-local evil-motion-state-modes nil)
    (setq-local evil-insert-state-modes nil)
    (setq-local evil-normal-state-modes nil))
  (defmacro eriks/evil-define-inner-local-textobject (key func)
    "binds key to text object func buffer-locally (mostly for my fork of evil-surround)"
    `(progn
       (define-key evil-visual-state-local-map   (kbd ,(concat "i " key)) ,func)
       (define-key evil-operator-state-local-map (kbd ,(concat "i " key)) ,func)))
  (defmacro eriks/evil-define-outer-local-textobject (key func)
    "binds key to text object func buffer-locally (mostly for my fork of evil-surround)"
    `(progn
       (define-key evil-visual-state-local-map   (kbd ,(concat "a " key)) ,func)
       (define-key evil-operator-state-local-map (kbd ,(concat "a " key)) ,func)))
  (eriks/hotfix
   'evil
   (defun eriks/paste-from-register (register)
     "Copy of `evil-paste-from-register', except this is not
moving the cursor one step too far."
     (interactive
      (let ((overlay (make-overlay (point) (point)))
            (string "\""))
        (unwind-protect
            (progn
              ;; display " in the buffer while reading register
              (put-text-property 0 1 'face 'minibuffer-prompt string)
              (put-text-property 0 1 'cursor t string)
              (overlay-put overlay 'after-string string)
              (list (or evil-this-register (read-char))))
          (delete-overlay overlay))))
     (evil-paste-before nil register t))))

(use-package golden-ratio-scroll-screen
  :ensure t
  :general
  ('motion
   "C-d" 'golden-ratio-scroll-screen-up
   "C-u" 'golden-ratio-scroll-screen-down)
  ('visual
   "C-d" 'evil-scroll-down
   "C-u" 'evil-scroll-up)
  ;; ;TODO: why do i have these?
  ;; ([remap scroll-down-command] 'golden-ratio-scroll-screen-down
  ;;  [remap scroll-up-command]   'golden-ratio-scroll-screen-up)
  )

(use-package drag-stuff
  :ensure t
  :general
  ('(normal visual)
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down
   "M-b" 'drag-stuff-left
   "M-f" 'drag-stuff-right))

(use-package eriks-evil-drag-line
  :general
  ('(normal visual)
   "M-h" 'eriks/evil-drag-line-left
   "M-l" 'eriks/evil-drag-line-right))

(use-package evil-args
  :ensure t
  :general
  ('inner
   "a" 'evil-inner-arg)
  ('outer
   "a" 'evil-outer-arg))

(use-package eriks-evil-open-join-line
  :general
  ('normal
   "S" 'eriks/evil-open-line
   "<return>" 'eriks/evil-open-line-below
   "S-<return>" 'eriks/evil-open-line-above
   "go" 'eriks/evil-open-below-comment)
  ('(normal visual)
   "gJ" 'eriks/evil-join-no-space
   "J"  'eriks/evil-join-no-comment
   "K"  'eriks/evil-join-no-comment-backward))

(use-package eriks-evil-random
  :config
  (advice-add 'evil-record-macro :before #'eriks/evil-better-record-macro)
  :general
  ('normal
   "g8" 'eriks/evil-what-cursor))

(use-package evil-nerd-commenter
  :ensure t
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
  :general
  ('(normal visual)
   "gp" 'evil-operator-clone
   "gr" 'evil-operator-eval))

(use-package evil-surround
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
  :general
  ('visual
   :prefix eriks/leader
   :infix "i"
   "n" 'eriks/evil-column-numbers-insert-numbers
   "z" 'eriks/evil-column-numbers-insert-numbers-zero
   "l" 'eriks/evil-column-numbers-insert-letters
   "i" 'eriks/evil-column-numbers-insert))

(use-package eriks-delete-empty-parens
  :general
  ('insert
   "C-s" 'eriks/delete-empty-parens))

(use-package eriks-line-cleanup
  :general
  ('(normal insert)
   "C-a" 'eriks/line-cleanup-dwim))

(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

(use-package eriks-evil-line-to
  :general
  ('inner
   "l" 'eriks/evil-inside-line-text-object)
  ('outer
   "l" 'eriks/evil-outside-line-text-object))

(use-package eriks-fix-last-shift-mistake
  :general
  ('(insert normal)
   "M-c" 'eriks/fix-last-shift-mistake))

(use-package evil-lion
  :ensure t
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

(use-package eriks-evil-add-to-search-history
  :disabled
  :general
  ('normal
   :prefix eriks/leader
   "s" 'eriks/evil-add-to-search-history))

(use-package eriks-evil-highlight
  :general
  ('(normal visual)
   :prefix eriks/leader
   "h" 'eriks/evil-highlight
   "H" 'eriks/evil-unhighlight))

(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

(use-package eriks-evil-visual-shift
  :general
  ('visual
   "<" 'eriks/evil-shift-left-visual
   ">" 'eriks/evil-shift-right-visual))

(use-package evil-collection
  :ensure t)
