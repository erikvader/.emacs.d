(defun eriks/evil-drag-line-right (dir)
  "Runs `indent-rigidly-right' on the current line or on the current
region. If DIR is negative then run `indent-rigidly-left' instead.

This can be thought of as \"dragging\" the contents of the selected
lines left or right."
  (interactive "p")
  (let* ((is-visual (evil-visual-state-p))
         (evil-range (evil-visual-range))
         (line (bounds-of-thing-at-point 'line))
         (beg (if is-visual
                  (car evil-range)
                (car line)))
         (end (if is-visual
                  (cadr evil-range)
                (cdr line))))
    (if (> dir 0)
        (indent-rigidly-right beg end)
      (indent-rigidly-left beg end))
    (when is-visual
      (evil-normal-state)
      (evil-visual-restore))))

(defun eriks/evil-drag-line-left (dir)
  "Runs `eriks/evil-drag-line-right' with a negative argument."
  (interactive "p")
  (eriks/evil-drag-line-right (- dir)))

(provide 'eriks-evil-drag-line)
