(evil-define-text-object eriks/evil-inside-line-text-object (count &optional beg end type)
  "Text object to select everything in a line but ignoring leading whitespace."
  (save-excursion
    (evil-first-non-blank)
    (let ((first (point)))
      (evil-end-of-line)
      (evil-range first (point)))))

(evil-define-text-object eriks/evil-outside-line-text-object (count &optional beg end type)
  "Text object to select everything in a line including leading whitespace."
  (save-excursion
    (move-beginning-of-line nil)
    (let ((first (point)))
      (evil-end-of-line)
      (evil-range first (point)))))

(provide 'eriks-evil-line-to)
