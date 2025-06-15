(require 'evil-digraphs) ;; for `eriks/evil-what-cursor'

(defun eriks/evil-what-cursor (&optional arg)
  "Extension of `what-cursor-position' that also shows how the
character can be inserted (if possible) with `evil-insert-digraph'"
  (interactive "P")
  (message "")
  (what-cursor-position arg)
  (let ((s (cl-find
            (following-char)
            (append evil-digraphs-table-user evil-digraphs-table)
            :key #'cdr)))
    (when s
      (princ (format "%s C-k %c%c => %c"
                     (or (current-message) ":(")
                     (caar s)
                     (cadar s)
                     (cdr s))))))

(defun eriks/evil-better-record-macro (_)
  "When done recording a macro, set `evil-last-register' to the newly
recorded register so that `evil-execute-macro' will believe that this
macro was the last executed one."
  (when (and evil-this-macro defining-kbd-macro)
    (setq evil-last-register evil-this-macro)))

(evil-define-motion eriks/evil-goto-last-non-empty-line (_count)
  "Similar to `evil-goto-line', but goes to the last non-empty
line instead. Preserves column placement."
  :jump t
  :type line
  (let* ((cur-line (line-number-at-pos))
         (last (point-max))
         (last-non-empty (cl-dotimes (i last)
                           (when-let ((c (char-before (- last i)))
                                      (_ (not (= ?\n c))))
                             (cl-return i)))))
    (when last-non-empty
      (line-move (- (line-number-at-pos (- last last-non-empty))
                    cur-line)))))

(provide 'eriks-evil-random)
