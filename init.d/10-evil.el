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
   "p" 'evil-forward-paragraph
   "e" 'next-error)
  ('motion
   :prefix "["
   "s" 'evil-backward-sentence-begin
   "}" 'evil-backward-section-end
   "{" 'evil-backward-section-begin
   "p" 'evil-backward-paragraph
   ;; BUG: it doesn't seem like these work with flycheck, it seems like xref is taking
   ;; precedence, i think
   "e" 'previous-error
   "E" 'first-error)
  ;;NOTE: make it more emacsy by setting keybinds like the minibuffer
  ('evil-command-line-map
   "<up>" 'previous-line-or-history-element
   "<down>" 'next-line-or-history-element
   "C-a" nil ;; let through beginning of line, completion is bound to many other keys
   "C-k" nil ;; let through kill line, not using digraphs
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('normal
   "+" "`[v`]"
   "|" (general-simulate-key ('evil-execute-macro "@")
         ;; Prefix args actually work compared to a macro, i.e., binding to "@@"
         :docstring "Shorthand for executing the last macro, aka @@")
   "U" 'evil-redo)
  ('inner
   "d" 'evil-inner-defun)
  ('outer
   "d" 'evil-a-defun)
  :config
  (evil-mode 1)
  ;; NOTE: Doesn't work to set these in :custom, They overwrite later calls
  ;; to `evil-set-initial-state' for some reason.
  (setq-default evil-emacs-state-modes nil
                evil-motion-state-modes nil
                evil-insert-state-modes nil
                evil-normal-state-modes '(prog-mode text-mode)
                evil-emacs-state-cursor '(hollow))

  (defface eriks/evil-ex-search-cursor nil
    "Face of the cursor when searching with `evil-ex-search-forward'")
  (define-advice evil-ex-start-search (:around (fun &rest args) make-cursor-more-visible)
    "Make the cursor actually visible when incrementally searching.

This advice applies the face `eriks/evil-ex-search-cursor' when
searching and makes it into a box. This will unfortunately affect all
buffers since there doesn't seem to be a way to change the cursor color
buffer locally, and the cursor type is changed by
`cursor-in-non-selected-windows'."
    (if (not (and evil-ex-search-interactive
                  evil-ex-search-incremental))
        (apply fun args)
      (let* ((cursor-in-non-selected-windows 'box)
             (org-color (face-attribute 'cursor :background))
             (new-color (if-let* ((color (face-attribute 'eriks/evil-ex-search-cursor :background))
                                  ((not (eq 'unspecified color))))
                            color
                          org-color)))
        (set-face-attribute 'cursor nil :background new-color)
        (unwind-protect
            (apply fun args)
          (set-face-attribute 'cursor nil :background org-color)))))

  (evil-define-text-object evil-inner-defun (count &optional beg end _type)
    "Select inner defun, aka section."
    :type line
    (evil-select-inner-object 'evil-defun beg end type count t))

  (evil-define-text-object evil-a-defun (count &optional beg end _type)
    "Select outer defun, aka section."
    :type line
    ;;NOTE: an outer variant is not possible?
    ;;https://github.com/emacs-evil/evil/issues/874, so the end whitespace is added
    ;;manully.
    (when-let* ((range (evil-select-inner-object 'evil-defun beg end type count t)))
      (eriks/evil-select-outer-whitespace-lines range)))

  (defun eriks/evil-select-outer-whitespace-lines (range)
    "Extends an evil range to include surrounding empty lines.

This mimics what `evil-select-an-object' does, but only works on
whitespace. The normal one tries to figure out a non-thing, but that
doesn't work for all things.

The type of the range should be line."
    (let ((org-end (evil-range-end range))
          (org-beg (evil-range-beginning range))
          (regex "\\(?:[[:space:]]*\n\\)+"))
      (evil-set-range-end range
                          (save-excursion
                            (goto-char org-end)
                            (or (re-search-forward regex nil t)
                                org-end)))
      (when (= org-end (evil-range-end range))
        (evil-set-range-beginning range
                                  (save-excursion
                                    (goto-char org-beg)
                                    (or (re-search-backward regex nil t)
                                        org-beg)))))
    range)

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
    "C-w" 'rename-visited-file
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
    (message "Using register: %c" register))

  (define-advice evil-ex-search-forward (:after (&rest args) extra-docs)
    "Extra documentation for evil search.

Every slash can be a question mark to go the other way.

/pattern
/pattern/offset

The offset can contain a semicolon, in which a new search can be
started, can be chained more than once.

/pattern1/;?pattern2
/pattern1/1;/pattern2

Pattern can be omitted to mean the last used pattern, i.e. //

pattern:
  - Can contain the flags \\c and \\C to force case-insensitivity and case-sensitivity,
    respectively. Otherwise the default is used as given by `evil-ex-search-case'.

offset:
  - [num]: num lines downward, can be negative
  - e[num]: num characters to the right of the end, can be negative or omitted
  - s[num]: num characters to the right of the start, can be negative or omitted"
    nil)

  (define-advice evil-ex-substitute (:after (&rest args) extra-docs)
    "Extra documentation for evil substitute.

:[range]s/pattern/string/flags

range:
  - nothing or .: current line
  - a,b: range from a to b, and the cursor is left on the current line to calculate b
  - a;b: range from a to b, but the cursor is moved to a before calculating b
  - [range]{- or +}[num]: move cursor num lines up or down from another range or the
    current line
  - number: absolute line
  - $: last line
  - %: the entire buffer
  - '<,'> or *: current selection
  - '[,']: most recent yank (paste)
  - /pattern/: the next line the matches pattern
  - 'x: the line where mark x is

pattern:
  - Normal regex, much the same as `evil-ex-search-forward'.
  - Smart case is used by default, according to `evil-ex-substitute-case'.

string:
  - Replacement string
  - \\[num]: replace with the nth capture group.
  - &: same as \\0, i.e. the whole match
  - ~: use last replacement string
  - The replacement will match the case of the search ala `case-replace'. Disable this
    behaviour be using the `I' flag or \\C, i.e. make the search case-sensitive. This also
    messes with \\l and friends, they don't always work with case matching enabled.
  - \\u and \\l: make the next character upper or lower case
  - \\U and \\L: make all subsequent characters upper or lower case, until a \\e or \\E is
    encountered.
  - \\t and \\n: insert tab and newline

flags:
  - &: use the previous flags
  - c: confirm each substitution
  - g: replace all occurences in the line, not just the first
  - i: ignore case, case-insensitive
  - I: don't ignore case, case-sensitive
  - n: count occurences instead of substituting
  - p: print the line containing the last subsitute
  - #: the same as p, but also print the line number"
    nil)

  (define-advice evil-ex-global (:after (&rest args) extra-docs)
    "Extra documentation for evil global.

:[range]g/pattern/cmd

Runs cmd on each line matching pattern. Use g! or v to invert the
pattern, i.e. run command on non-matching lines.

range:
  - Same as for `evil-ex-substitute'.

pattern:
  - Normal regex, much the same as `evil-ex-search-forward'.

cmd:
  - The ex command to run on each matching line
  - The default command is p(rint)
  - Another useful command is d(elete)
  - Use normal to run a normal command, like :g/asd/normal Ihej
  - Globals can be chained, like g/include/v/exclude/p, but it doesn't seem to work in
    evil."
    nil))

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
  :config
  (eriks/leader-def 'normal
    "C-e" 'evil-operator-eval
    "M-e" 'evil-operator-eval-replace)
  :general-config
  ('normal
   "gp" 'evil-operator-clone))

(use-package eriks-line-cleanup
  :general-config
  ('normal
   "C-a" 'eriks/line-cleanup-dwim))

(use-package evil-exchange
  :ensure t
  :custom
  (evil-exchange-highlight-face 'region)
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
  ;; NOTE: This is pinned because the commit after introduces an annoying switch that
  ;; changes how RET works in REPL modes. I want RET to send the input to the repl both in
  ;; normal and insert states, and i don't like that they implemented it by binding to
  ;; [return] and stuff, only RET is enough.
  ;; https://github.com/emacs-evil/evil-collection/issues/904
  ;; https://github.com/emacs-evil/evil-collection/commit/14c09ec65c0d6184115233741f5667a1de5a7f6b
  :vc (:url https://github.com/emacs-evil/evil-collection.git
            :rev "c214d48dd80d5ba9b7d05b7751d67b9281cd25f4")
  :custom
  (evil-collection-key-blacklist (list eriks/leader)))

(use-package evil-numbers
  ;; NOTE: use `rectangle-number-lines' for cases this doesn't support, like inserting
  ;; letter a through z
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
  (add-to-list 'evil-owl-register-groups '("Ex" . (?\C-w ?\C-a ?\C-o ?\C-f)) t)
  (evil-owl-mode 1))

(use-package eriks-evil-symbol-motions
  :disabled ;; NOTE: I never use these
  :general-config
  ('motion
   "M-w" 'eriks/evil-forward-symbol-begin
   "M-b" 'eriks/evil-backward-symbol-begin
   "M-e" 'eriks/evil-forward-symbol-end))
