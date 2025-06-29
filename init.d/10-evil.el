(use-package evil
  :ensure t
  :init
  ;;TODO: create custom general definer, and put everything in a keymap so it can be listed later
  ;;TODO: define-prefix-command
  ;;TODO: se över alla bindings. Man behöver inte lägga bindings i visual om de också är med i normal
  ;;https://github.com/noctuid/evil-guide?tab=readme-ov-file#leader-key
  (defconst eriks/leader "SPC" "My leader key for evil")
  :custom
  (evil-default-state 'emacs)
  (evil-ex-visual-char-range t)
  (evil-search-module 'evil-search)
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  (evil-goto-definition-functions '(evil-goto-definition-xref evil-goto-definition-search))
  :general-config
  ('emacs "<escape>" 'evil-exit-emacs-state)
  ('motion
   "M-e" 'evil-forward-sentence-begin
   "M-a" 'evil-backward-sentence-begin
   "M-f" 'evil-forward-section-end
   "M-b" 'evil-backward-section-begin)
  ;;NOTE: make it more emacsy
  ('evil-ex-completion-map
   "C-a" nil
   "C-k" nil
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "<up>" 'ignore
   "<left>" 'ignore
   "<down>" 'ignore
   "<right>" 'ignore)
  ('normal
   "|" (general-simulate-key ('evil-execute-macro "@")
         ;; Prefix args actually work compared to a macro, i.e., binding to "@@"
         :docstring "Shorthand for executing the last macro, aka @@")
   "<backspace>" 'evil-ex-nohighlight
   "U" 'evil-redo)
  ('global
    [remap backward-kill-word] 'evil-delete-backward-word
    [remap backward-word] 'evil-backward-word-begin
    [remap forward-word] 'evil-forward-word-begin
    ;; [remap kill-word] 'evil-delete-forward-word ;;TODO: something like this doesn't exist
    )
  ('inner
   "d" 'evil-inner-defun)
  :config
  (evil-mode 1)
  ;; NOTE: Doesn't work to set these in :custom, They overwrite later calls
  ;; to `evil-set-initial-state' for some reason.
  (setq-default evil-emacs-state-modes nil
                evil-motion-state-modes '(help-mode)
                evil-insert-state-modes nil
                evil-normal-state-modes '(prog-mode conf-mode text-mode)
                evil-emacs-state-cursor '(hollow))

  ;;TODO: evaluate if this weird function is actually/still needed
  (defun eriks/force-emacs-initial-state ()
    "A handy way to set initial state for a minor mode. The buffer's
normal initial state is ignored."
    (evil-emacs-state)
    (setq-local evil-buffer-regexps nil)
    (setq-local evil-motion-state-modes nil)
    (setq-local evil-insert-state-modes nil)
    (setq-local evil-normal-state-modes nil))

  (evil-define-text-object evil-inner-defun (count &optional beg end _type)
    "Select inner defun."
    ;;NOTE: an outer variant is not possible? https://github.com/emacs-evil/evil/issues/874
    (evil-select-inner-object 'evil-defun beg end type count))

  (general-create-definer eriks/leader-def
    :prefix eriks/leader)

  (eriks/leader-def 'normal
    "." 'repeat
    "r" 'revert-buffer
    ;;TODO: make this work like my dirvish rename (cw), specifically with the autocomplete prompt
    "R" 'rename-visited-file
    "C-o" 'browse-url-at-point
    "q" 'kmacro-insert-counter
    "Q" 'kmacro-set-counter
    "&" 'evil-ex-repeat-substitute-with-flags)

  ;;TODO: use general to bind these instead? Create keymap shortcuts for local modes if
  ;;not already exists?
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

(use-package drag-stuff
  :ensure t
  :general-config
  ('normal
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down))

(use-package eriks-evil-drag-line
  :general-config
  ('normal
   "M-h" 'eriks/evil-drag-line-left
   "M-l" 'eriks/evil-drag-line-right
   "M-H" 'eriks/evil-indent-line-left
   "M-L" 'eriks/evil-indent-line-right))

(use-package evil-args
  :ensure t
  :general-config
  ('inner
   "a" 'evil-inner-arg)
  ('outer
   "a" 'evil-outer-arg))

(use-package eriks-evil-open-join-line
  :general-config
  ('normal
   "S" 'eriks/evil-open-line
   "RET" 'eriks/evil-open-line-below
   "S-<return>" 'eriks/evil-open-line-above
   "go" 'eriks/evil-open-below-comment
   "gJ" 'eriks/evil-join-no-space
   "J"  'eriks/evil-join-no-comment
   "K"  'eriks/evil-join-no-comment-backward))

(use-package eriks-evil-random
  :config
  (advice-add 'evil-record-macro :before #'eriks/evil-better-record-macro)
  :general-config
  ('normal
   "g8" 'eriks/evil-what-cursor)
  ('motion
   "gG" 'eriks/evil-goto-last-non-empty-line))

(use-package evil-nerd-commenter
  :ensure t
  :general-config
  ('normal
   "gc" 'evilnc-comment-operator
   "gC" 'evilnc-copy-and-comment-operator)
  ('inner
   "c" 'evilnc-inner-comment)
  ('outer
   "c" 'evilnc-outer-commenter))

(use-package evil-extra-operator
  :ensure t
  :general-config
  ('normal
   "gp" 'evil-operator-clone))

;;TODO: stop using my fork since i never use any of the functionality i added
;;TODO: make deleting closing paren cleanup whitespace, do not leave empty lines where the delimeter was
;;TODO: evil-embrace?
(use-package evil-surround
  :disabled
  :general-config
  ('visual
   "s" 'evil-surround-region
   "S" 'evil-Surround-region)
  ('normal
   "gs" 'evil-surround-edit
   "gS" 'evil-Surround-edit)
  :gfhook
  ;;TODO: move this to the latex use-package
  ('LaTeX-mode-hook (cl-defun latex-evil-surround-hook ()
                      (add-to-list 'evil-surround-pairs-alist '(?$ . ("$" . "$")))
                      (add-to-list 'evil-surround-pairs-alist '(?m . latex-surround-env)))))

(use-package eriks-evil-column-numbers
  :config
  (eriks/leader-def 'visual
    :infix "i"
    "n" 'eriks/evil-column-numbers-insert-numbers
    "z" 'eriks/evil-column-numbers-insert-numbers-zero
    "l" 'eriks/evil-column-numbers-insert-letters
    "i" 'eriks/evil-column-numbers-insert))

(use-package eriks-delete-empty-parens
  :general-config
  ('insert
   "C-s" 'eriks/delete-empty-parens))

(use-package eriks-line-cleanup
  :general-config
  ('normal
   "C-a" 'eriks/line-cleanup-dwim))

(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

(use-package eriks-evil-line-to
  :general-config
  ('inner
   "l" 'eriks/evil-inside-line-text-object)
  ('outer
   "l" 'eriks/evil-outside-line-text-object))

(use-package eriks-fix-last-shift-mistake
  :general-config
  ('(insert normal)
   "M-c" 'eriks/fix-last-shift-mistake))

(use-package evil-lion
  :ensure t
  :general-config
  ('normal
   :prefix "g"
   "a" 'evil-lion-left
   "A" 'evil-lion-right))

;;TODO: use the successor evil-tex
;;TODO: move this to the rest of the latex config
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

(use-package eriks-evil-highlight
  :config
  (eriks/leader-def 'normal
   "h" 'eriks/evil-search-highlight-current-symbol))

(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-collection
  :ensure t
  :custom
  (evil-collection-key-blacklist (list eriks/leader)))

(use-package evil-numbers
  :ensure t
  :custom
  (evil-numbers-pad-default t)
  :config
  (eriks/defkey-repeat eriks-evil-numbers
    :states 'normal
    :prefix eriks/leader
    "C-a" 'evil-numbers/inc-at-pt
    "C-x" 'evil-numbers/dec-at-pt)
  :general-config
  (eriks/leader-def 'normal
    :infix "g"
    "C-a" 'evil-numbers/inc-at-pt-incremental
    "C-x" 'evil-numbers/dec-at-pt-incremental))

(use-package eriks-evil-default-register
  :general-config
  (eriks/leader-def 'normal
    "p" 'eriks/evil-paste-after
    "P" 'eriks/evil-paste-before
    "d" 'eriks/evil-yank-delete))

(use-package eriks-evil-backward-exclusive
  :config
  (eriks/defkey-repeat eriks-evil-backward
    :states 'motion
    :prefix "g"
    "e" 'eriks/evil-backward-word-end-exclusive
    "E" 'eriks/evil-backward-WORD-end-exclusive))
