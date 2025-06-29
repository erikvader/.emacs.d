;;TODO: how to make it decrease?
(defun eriks/evil-column-numbers-insert (beg end start format)
  "Inserts increasing numbers in a column. The values start on START
  and are formatted with `format' using the format."
  (interactive (list
                (progn
                  (unless (evil-visual-state-p)
                    (user-error "Must have a region active"))
                  (region-beginning))
                (region-end)
                (read-number "Start number: " 1)
                (read-string "Format (default %02d): " nil nil (list "%02d" "%X" "%c"))))
  (rectangle-number-lines beg end start format))

(defun eriks/evil-column-numbers-insert-numbers (beg end)
  "Inserts numbers in a column starting at 1."
  (interactive "r")
  (eriks/evil-column-numbers-insert beg end 1 "%d"))

(defun eriks/evil-column-numbers-insert-numbers-zero (beg end)
  "Inserts zero-padded numbers in a column starting at 1."
  (interactive "r")
  (eriks/evil-column-numbers-insert beg end 1 "%02d"))

(defun eriks/evil-column-numbers-insert-letters (beg end)
  "Inserts small letters in a column starting at 'a'."
  (interactive "r")
  (eriks/evil-column-numbers-insert beg end 97 "%c"))

(provide 'eriks-evil-column-numbers)
