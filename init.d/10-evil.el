(use-package evil
  :ensure t
  :init
  (defconst eriks/leader "SPC" "My leader key for evil")
  :custom
  (evil-default-state 'emacs)
  (evil-ex-visual-char-range t)
  (evil-search-module 'evil-search)
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  (evil-goto-definition-functions '(evil-goto-definition-xref evil-goto-definition-search))
  (evil-symbol-word-search t)
  (evil-ex-search-persistent-highlight nil)
  :general-config
  ('emacs "<escape>" 'evil-exit-emacs-state)
  ('motion
   "M-d" 'eriks/evil-scroll-down-half-other-window
   "M-u" 'eriks/evil-scroll-up-half-other-window
   "-" 'repeat
   "<backspace>" 'evil-ex-nohighlight
   "gr" 'revert-buffer)
  ('motion
   :prefix "]"
   "s" 'evil-forward-sentence-begin
   "}" 'evil-forward-section-end
   "{" 'evil-forward-section-begin
   "p" 'evil-forward-paragraph)
  ('motion
   :prefix "["
   "s" 'evil-backward-sentence-begin
   "}" 'evil-backward-section-end
   "{" 'evil-backward-section-begin
   "p" 'evil-backward-paragraph)
  ;;NOTE: make it more emacsy
  ('evil-ex-completion-map
   "C-a" nil ;; let through beginning of line
   "C-k" nil ;; let through kill line
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "C-SPC" 'completion-at-point)
  ('normal
   "|" (general-simulate-key ('evil-execute-macro "@")
         ;; Prefix args actually work compared to a macro, i.e., binding to "@@"
         :docstring "Shorthand for executing the last macro, aka @@")
   "U" 'evil-redo)
  ('global
   [remap scroll-down-command] 'evil-scroll-up
   [remap scroll-up-command] 'evil-scroll-down
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
                evil-motion-state-modes nil
                evil-insert-state-modes nil
                evil-normal-state-modes '(prog-mode text-mode)
                evil-emacs-state-cursor '(hollow))

  (evil-define-text-object evil-inner-defun (count &optional beg end _type)
    "Select inner defun."
    ;;NOTE: an outer variant is not possible? https://github.com/emacs-evil/evil/issues/874
    (evil-select-inner-object 'evil-defun beg end type count))

  ;;NOTE: `scroll-other-window' doesn't have the scroll-command property
  (defun eriks/evil-scroll-down-half-other-window (&optional lines)
    (interactive "P")
    (with-selected-window (other-window-for-scrolling)
      (evil-scroll-down lines)))

  (defun eriks/evil-scroll-up-half-other-window (&optional lines)
    (interactive "P")
    (with-selected-window (other-window-for-scrolling)
      (evil-scroll-up lines)))

  (general-create-definer eriks/leader-def
    :prefix eriks/leader)

  (eriks/leader-def 'normal
    "i" 'imenu
    "." 'evil-ex-repeat
    "R" 'rename-visited-file
    "C-u" 'universal-argument
    "q" 'kmacro-insert-counter
    "Q" 'kmacro-set-counter)

  (eriks/leader-def 'normal
    :infix "e"
    "m" 'evil-show-marks
    "r" 'evil-show-registers
    "j" 'evil-show-jumps)

  (eriks/leader-def 'normal
    :infix "o"
    "u" 'browse-url-at-point)

  (define-advice evil-use-register (:after (register) echo)
    "Echo the chosen register and hope evil will support this natively soon."
    ;;TODO: echo current evil commands in the minibuffer https://github.com/emacs-evil/evil/issues/1755
    (message "Using register: %c" register)))

(use-package drag-stuff
  :ensure t
  :general-config
  ('normal
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down))

(use-package eriks-evil-drag-line
  :general-config
  ('normal
   "M-H" 'eriks/evil-drag-line-left
   "M-L" 'eriks/evil-drag-line-right
   "M-h" 'eriks/evil-indent-line-left
   "M-l" 'eriks/evil-indent-line-right))

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

(use-package eriks-evil-column-numbers
  :config
  (eriks/leader-def 'visual
    :infix "i"
    "n" 'eriks/evil-column-numbers-insert-numbers
    "z" 'eriks/evil-column-numbers-insert-numbers-zero
    "l" 'eriks/evil-column-numbers-insert-letters
    "i" 'eriks/evil-column-numbers-insert))

(use-package eriks-line-cleanup
  :general-config
  ('normal
   "C-a" 'eriks/line-cleanup-dwim))

;; TODO: make the selection more visible
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

(use-package eriks-evil-backward-exclusive
  :general-config
  ('motion
   :prefix "g"
   "e" 'eriks/evil-backward-word-end-exclusive
   "E" 'eriks/evil-backward-WORD-end-exclusive))

(use-package evil-quickscope
  :ensure t
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-owl
  :ensure t
  :diminish
  :config
  (evil-owl-mode 1))

(use-package eriks-evil-symbol-motions
  :general-config
  ('motion
   "M-w" 'eriks/evil-forward-symbol-begin
   "M-b" 'eriks/evil-backward-symbol-begin
   "M-e" 'eriks/evil-forward-symbol-end))
