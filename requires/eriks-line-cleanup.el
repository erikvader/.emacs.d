(defun eriks/delete-trailing-this-line ()
  "Removes trailing whitespace from the current line."
  (interactive)
  (let* ((l (bounds-of-thing-at-point 'line))
         (beg (car l))
         (end (cdr l)))
    (when (and end beg)
      (save-excursion
        (goto-char (1- end))
        (skip-chars-backward "\t " beg)
        (when (looking-at-p "[\t ]")
          (delete-region (point) (if (eq (char-before end) ?\n)
                                     (1- end)
                                   end)))))))

(defun eriks/line-cleanup-dwim ()
  " - if the mark is active, then remove trailing whitespace inside region
 - if the line is empty, then indent it to the same level as the previous line
 - if the line is not empty and only contains indentation, then delete the line
 - if the line is not empty and contains non-indentation characters, then remove trailing whitespace"
  (interactive)
  (if (use-region-p)
      (progn
        (delete-trailing-whitespace (region-beginning) (region-end))
        (deactivate-mark))
    (let* ((l (bounds-of-thing-at-point 'line))
           (i (current-indentation))
           (non-i (- (- (cdr l) (car l)) i 1)))
      (cond
       ((or (and (> i 0) (<= non-i 0)) ; only indentation
            (> non-i 0))              ; contains non-indentation
        (eriks/delete-trailing-this-line))
       ((and (= i 0) (= non-i 0))     ; completely empty line
        (save-excursion
          (when (= 0 (forward-line -1))
            (setq i (current-indentation))))
        (indent-to i))))))

(provide 'eriks-line-cleanup)
