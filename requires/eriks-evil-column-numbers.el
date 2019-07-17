(defun eriks/evil-column-numbers-insert (&optional start flags padding specifier)
  "Inserts increasing numbers in a column. The values start on START
  and are formatted with `format' using the format string
  \"%[FLAGS][PADDING][SPECIFIER]\"."
  (interactive)
  (unless (and (evil-visual-state-p)
               (eq evil-visual-selection 'block))
    (error "no block selection active"))
  ;; NOTE: prettiest/best way to do this?
  (let ((start (or start (read-number "Start number: " 1)))
        (flags (or flags (read-string "Flags (default \"\"): " nil nil "")))
        (padding (or padding
                     (and current-prefix-arg (prefix-numeric-value))
                     (read-number "Amount of padding: " 0)))
        (specifier (or specifier
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
             specifier))))

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
