(defun eriks/evil-column-numbers-insert (beg end start flags padding specifier)
  "Inserts increasing numbers in a column. The values start on START
  and are formatted with `format' using the format string
  \"%[FLAGS][PADDING][SPECIFIER]\"."
  (interactive (list
                (region-beginning)
                (region-end)
                (read-number "Start number: " 1)
                (read-string "Flags (default \"\"): " nil nil "")
                (or (and current-prefix-arg
                         (prefix-numeric-value current-prefix-arg))
                    (read-number "Amount of padding: " 0))
                (read-string "Specifier (default \"d\"): " nil nil "d")))
  (rectangle-number-lines
   beg
   end
   start
   (concat "%"
           (or flags "")
           (if (and padding
                    (numberp padding)
                    (> padding 0))
               (number-to-string padding)
             "")
           specifier)))

(defun eriks/evil-column-numbers-insert-numbers (beg end arg)
  "Inserts numbers in a column starting at 1. Prefix argument is the
amount of padding to use."
  (interactive "r\np")
  (eriks/evil-column-numbers-insert beg end 1 nil arg "d"))

(defun eriks/evil-column-numbers-insert-numbers-zero (beg end arg)
  "Inserts zero-padded numbers in a column starting at 1. Prefix
argument is the amount of padding to use."
  (interactive "r\np")
  (eriks/evil-column-numbers-insert beg end 1 "0" arg "d"))

(defun eriks/evil-column-numbers-insert-letters (beg end arg)
  "Inserts small letters in a column starting at 'a'. Prefix argument
is the amount of padding to use."
  (interactive "r\np")
  (eriks/evil-column-numbers-insert beg end 97 nil arg "c"))

(provide 'eriks-evil-column-numbers)
