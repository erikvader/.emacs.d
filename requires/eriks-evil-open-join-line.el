;; command for splitting a line intelligently and commands for joining
;; lines with different behaviours

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

(defmacro eriks/evil--join-template (name doc &rest body)
  "Creates an evil operator named 'eriks/evil-join-{name}' that runs
BODY after each line is joined.

With no BODY this is exactly the same as `evil-join'.

The first value in BODY can be the special value :backwards. If this
value is present, then the current line is joined to the one above
instead of the one below."
  (let* ((backwards (eq (car body) :backwards))
         (body (or (when (eq (car body) :backwards)
                     (cdr body))
                   body)))
    `(evil-define-operator ,(intern (concat "eriks/evil-join-" (symbol-name name))) (beg end)
       ,(concat "Join the selected lines just as `evil-join', but with a twist!\n" doc)
       :motion evil-line
       (let ((count (count-lines beg end)))
         (when (> count 1)
           (setq count (1- count)))
         (goto-char ,(if backwards
                         '(if (= (char-before end) ?\n)
                              (1- end)
                            end)
                       'beg))
         (dotimes (var count)
           (join-line ,(not backwards))
           ,@body)))))

(defun eriks/evil--join-remove-comment ()
  "If below line was a comment then remove the comment delimeters as
specified in `comment-start-skip' and put one space between the
lines."
  (when (and (nth 4 (syntax-ppss)) ; if previous line is a comment
             (looking-at (concat "\\( *\\)" comment-start-skip)))
    (replace-match "\\1")
    (goto-char (match-beginning 0))
    (fixup-whitespace) ;; use `join-line's sub-procedure to fix up whitespace
    ))                 ;; again after comment delimiter is gone

(eriks/evil--join-template
 no-space
 "This one removes comments defined in `comment-start-skip' and
removes all space between."
 (eriks/evil--join-remove-comment)
 (just-one-space 0))

(eriks/evil--join-template
 no-comment
 "This one removes comments defined in `comment-start-skip' and leaves
space according to `fixup-whitespace'"
 (eriks/evil--join-remove-comment))

(eriks/evil--join-template
 no-comment-backward
 "This one is the same as `eriks/evil-join-no-comment' except that
this join the current line to the one above instead of below."
 :backwards
 (eriks/evil--join-remove-comment))

(provide 'eriks-evil-open-join-line)
