(defun eriks/delete-empty-parens ()
  "Delete an empty set om parens (anything in the parens syntax class).
Point can be immediately after the closing paren, inside the parens or
immediately before the opening paren."
  (interactive)
  (save-excursion
    (let ((done nil)
          (ok nil)
          (i 0))
      (while (and
              (not done)
              (< i 3))
        (when (looking-at-p "\\s(\\s)\\|\\s\"\\s\"\\|\\s/\\s/\\|\\s$\\s$\\|\\s|\\s|") ; delimeters, strings, character delimeter, paired delimeter, generic string delimiter
          (delete-region (point) (+ 2 (point)))
          (setq done t ok t))
        (setq i (1+ i))
        (setq done (or done
                       (not (ignore-errors (progn (backward-char) t))))))
      (when (not ok)
        (message "There wasn't anything to remove...")))))

(provide 'eriks-delete-empty-parens)
