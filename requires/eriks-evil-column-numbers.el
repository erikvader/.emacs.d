(defun eriks/evil-column-numbers-insert (start flags padding specifier)
  "Inserts increasing numbers in a column. The values start on START
  and are formatted with `format' using the format string
  \"%[FLAGS][PADDING][SPECIFIER]\"."
  (interactive (progn
                 (unless (and (evil-visual-state-p)
                              (eq evil-visual-selection 'block))
                   (error "no block selection active"))
                 (list (read-number "Start number: " 1)
                       (read-string "Flags (default \"\"): " nil nil "")
                       (or (and current-prefix-arg (prefix-numeric-value))
                           (read-number "Amount of padding: " 0))
                       (read-string "Specifier (default \"d\"): " nil nil "d"))))
  (rectangle-number-lines
   (region-beginning)
   (region-end)
   start
   (concat "%"
           (or flags "")
           (if (and padding
                    (numberp padding)
                    (> 0 padding))
               (number-to-string padding)
             "")
           specifier)))

(defun eriks/evil-column-numbers-insert-numbers (arg)
  "Inserts numbers in a column starting at 1. Prefix argument is the
amount of padding to use."
  (interactive "p")
  (eriks/evil-column-numbers-insert 1 nil arg "d"))

(defun eriks/evil-column-numbers-insert-numbers-zero (arg)
  "Inserts zero-padded numbers in a column starting at 1. Prefix
argument is the amount of padding to use."
  (interactive "p")
  (eriks/evil-column-numbers-insert 1 "0" arg "d"))

(defun eriks/evil-column-numbers-insert-letters (arg)
  "Inserts small letters in a column starting at 'a'. Prefix argument
is the amount of padding to use."
  (interactive "p")
  (eriks/evil-column-numbers-insert 97 nil arg "c"))

(provide 'eriks-evil-column-numbers)
