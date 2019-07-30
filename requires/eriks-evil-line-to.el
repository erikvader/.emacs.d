(require 'thingatpt)

(cl-defun eriks/bounds-of-line (&key include-indentation include-newline (cons-fun #'cons))
  "Just as `bounds-of-thing-at-point' with 'line as argument, this
function returns the bounds of the current line in a cons (beg . end).

This function differs by excluding any newline character at the end of
the line and by not including leading whitespace.

This function accepts the following options as keyword arguments:

:include-indentation  Include leading whitespace (default nil)
:include-newline      Include trailing newline (default nil)
:cons-fun             Function called with beginning and end position
                        of line to yield the return value of this
                        function (default #'`cons')
"
  (let* ((reg (bounds-of-thing-at-point 'line))
         (beg (if include-indentation
                  (car reg)
                (save-excursion
                  (back-to-indentation)
                  (point))))
         (end (if (and (not include-newline)
                       (= (char-before (cdr reg)) ?\n))
                  (1- (cdr reg))
                (cdr reg))))
    (funcall cons-fun beg end)))

(evil-define-text-object eriks/evil-inside-line-text-object (count &optional beg end type)
  "Text object to select everything in a line but ignoring leading whitespace."
  (eriks/bounds-of-line :cons-fun #'evil-range))

(evil-define-text-object eriks/evil-outside-line-text-object (count &optional beg end type)
  "Text object to select everything in a line including leading whitespace."
  (eriks/bounds-of-line :cons-fun #'evil-range
                        :include-indentation t))

(provide 'eriks-evil-line-to)
