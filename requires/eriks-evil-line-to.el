(require 'thingatpt)

(defun eriks/bounds-of-line (&rest keywords)
  "Just as `bounds-of-thing-at-point' with 'line as argument, this
function returns the bounds of the current line in a cons (beg . end).

This function differs by excluding any newline character at the end of
the line and by not including leading whitespace.

This function accepts the following options in KEYWORDS:

:include-indentation  Include leading whitespace
:evil-range           Return the result by calling `evil-range' instead
                      of `cons'
"
  (let* ((reg (bounds-of-thing-at-point 'line))
         (beg (if (memq :include-indentation keywords)
                  (car reg)
                (save-excursion
                  (back-to-indentation)
                  (point))))
         (end (if (= (char-before (cdr reg)) ?\n)
                  (1- (cdr reg))
                (cdr reg))))
    (if (memq :evil-range keywords)
        (evil-range beg end)
      (cons beg end))))

(evil-define-text-object eriks/evil-inside-line-text-object (count &optional beg end type)
  "Text object to select everything in a line but ignoring leading whitespace."
  (eriks/bounds-of-line :evil-range))

(evil-define-text-object eriks/evil-outside-line-text-object (count &optional beg end type)
  "Text object to select everything in a line including leading whitespace."
  (eriks/bounds-of-line :evil-range :include-indentation))

(provide 'eriks-evil-line-to)
