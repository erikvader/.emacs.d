;; small random utilites for evil

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
                     (cdr s))))))

(defun eriks/evil-better-record-macro (_)
  "When done recording a macro, set `evil-last-register' to the newly
recorded register so that `evil-execute-macro' will believe that this
macro was the last executed one."
  (when (and evil-this-macro defining-kbd-macro)
    (setq evil-last-register evil-this-macro)))
(advice-add 'evil-record-macro :before #'eriks/evil-better-record-macro)

(provide 'eriks-evil-random)
