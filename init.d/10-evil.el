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
  (evil-shift-width 3)
  (evil-want-C-d-scroll nil)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  :general
  ('emacs [escape] 'evil-normal-state)
  (evil-ex-completion-map
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  ('insert
   "<up>" 'noop
   "<left>" 'noop
   "<down>" 'noop
   "<right>" 'noop)
  ('normal
   "<backspace>" 'evil-ex-nohighlight)
  ('normal
   :prefix eriks/leader 
   "g 8" 'eriks/evil-what-cursor)
  :config
  (evil-mode 1)
  (setq-default evil-emacs-state-modes nil)
  (setq-default evil-motion-state-modes nil)
  (setq-default evil-insert-state-modes nil)
  (setq-default evil-normal-state-modes '(prog-mode))
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'org-mode 'normal)
  (evil-set-initial-state 'Man-mode 'motion)
  (evil-set-initial-state 'conf-mode 'normal)

  (defun eriks/evil-what-cursor (&optional arg)
    "Extension of `what-cursor-position' that also shows how the
character can be inserted (if possible) with `evil-insert-digraph'"
    (interactive "P")
    (message "")
    (what-cursor-position arg)
    (let ((s (find
              (following-char)
              (append evil-digraphs-table-user evil-digraphs-table)
              :key #'cdr)))
      (when s
        (princ (format "%s C-k %c%c => %c"
                       (or (current-message) ":(")
                       (caar s)
                       (cadar s)
                       (cdr s)))))))

(use-package golden-ratio-scroll-screen
  :ensure t
  :after evil
  :general
  ('normal
   "C-d" 'golden-ratio-scroll-screen-up
   "C-u" 'golden-ratio-scroll-screen-down)
  ('visual
   "C-d" 'evil-scroll-down
   "C-u" 'evil-scroll-up)
  ("M-u" 'universal-argument)
  (universal-argument-map
   "M-u" 'universal-argument-more
   "C-u" nil))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (global-undo-tree-mode 1)
  :general
  ('normal
   "U" 'undo-tree-redo)
  ('normal
   :prefix eriks/leader
   "u" 'undo-tree-visualize))

(use-package drag-stuff
  :ensure t
  :after evil
  :general
  ('(normal visual)
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down
   "M-h" 'drag-stuff-left
   "M-l" 'drag-stuff-right))

(with-eval-after-load 'evil
  (defvar evil-open-line-modes '((haskell-mode . same-as-prev)
                                 (prog-mode . according-to-mode))
    "Settings for `evil-open-line'.
Association list of the type (mode . action) where 'action' is done
if we are in major-mode 'mode'

'action' can be either:
  same-as-prev      = same indentation as the previous line
  according-to-mode = call `indent-according-to-mode'")

  (defun eriks/evil-open-line (ARG)
    "open-line for evil, designed to be the opposite of J (join-lines).

Indents the new line if it is not empty.
 - if prefix argument is a non-negative number, then indent that much
 - if prefix argument is raw, then invert the action from the following steps
 - if there is a match in `evil-open-line-modes', use that
 - else indent to the same amount as the previous line

Assumes `left-margin' is 0 or that there is no fill prefix (that
open-line doesn't indent the new line in any way)

If the line to be split is a comment, run `comment-indent-new-line'
instead (splits, adds comment chars and indents)."
    (interactive "P")
    (let ((start-ind (current-indentation))
          (raw (equal ARG '(4)))
          (in-comment (nth 4 (syntax-ppss)))
          method)
      (just-one-space 0)
      (if in-comment
          (save-excursion
            (comment-indent-new-line))
        (open-line 1)
        (save-excursion
          (forward-char)
          (unless (eolp)
            (if (and (numberp ARG) (>= ARG 0))
                (indent-to ARG)
              (setq method (or (cdr (cl-find-if #'derived-mode-p
                                                evil-open-line-modes
                                                :key 'car))
                               'same-as-prev))
              (when raw
                (setq method (if (eq method 'same-as-prev)
                                 'according-to-mode
                               'same-as-prev)))
              (cond ((eq method 'according-to-mode)
                     (indent-according-to-mode))
                    ((eq method 'same-as-prev)
                     (indent-to start-ind)))))))))

  (defun eriks/evil-open-line-above (ARG)
    "same as `evil-open-line' except that it is more like gO<esc>"
    (interactive "p")
    (if (bolp)
        (newline ARG)
      (save-excursion
        (beginning-of-line)
        (newline ARG))))

  (defun eriks/evil-open-line-below (ARG)
    "Same as `evil-open-line' except that it is more like go<esc>"
    (interactive "p")
    (save-excursion
      (end-of-line)
      (newline ARG)))

  (eriks/general-def-each
   ('normal
    "S" 'eriks/evil-open-line
    "<return>" 'eriks/evil-open-line-below
    "S-<return>" 'eriks/evil-open-line-above)))

(with-eval-after-load 'evil
  ;; copy of the normal evil-join
  (defmacro eriks/evil-join-template (name doc &rest BODY)
    "Creates an evil operator named 'eriks/evil-join-{name}' that runs
BODY after each time a line is joined."
    `(evil-define-operator ,(intern (concat "eriks/evil-join-" (symbol-name name))) (beg end)
       ,(concat "Join the selected lines just as `evil-join', but with a twist!\n" doc)
       :motion evil-line
       (let ((count (count-lines beg end)))
         (when (> count 1)
           (setq count (1- count)))
         (goto-char beg)
         (dotimes (var count)
           (join-line 1)
           ,@BODY))))

  (eriks/evil-join-template
   no-space
   "This one with no space inbetween"
   (just-one-space 0))

  (eriks/evil-join-template
   no-comment
   "This one removes comments defined in `comment-start-skip'"
   (when (and (nth 4 (syntax-ppss)) ; if previous line is a comment
              (looking-at (concat "\\( *\\)" comment-start-skip)))
     (replace-match "\\1")
     (goto-char (match-beginning 0))
     (just-one-space)))

  (eriks/general-def-each
   ('(normal visual)
    :prefix eriks/leader
    "J" 'eriks/evil-join-no-space)
   ('(normal visual)
    "J" 'eriks/evil-join-no-comment)
   ('(normal visual)
    "K" 'noop)))
