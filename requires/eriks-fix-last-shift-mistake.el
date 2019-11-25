(require 's)

(defun eriks/fix-last-shift-mistake ()
  "Fixes the last occurence of a shift mistake in the current word or
  capitalizes the word at point if it is all lowercase.

For example, if the user typed \"AWesome\" then a call to this
function would fix it to \"Awesome\"."
  (interactive)
  (let ((case-fold-search nil)
        beg
        end
        (bound (bounds-of-thing-at-point 'word)))
    (unless bound
      (user-error "Not inside a word"))
    (save-excursion
      (if (s-lowercase-p (buffer-substring (car bound) (cdr bound)))
          (capitalize-region (car bound) (cdr bound))
        (goto-char (car bound))
        (while (search-forward-regexp "[[:upper:]]+\\([[:upper:]]\\)" (cdr bound) t)
          (setq beg (match-beginning 1)
                end (match-end 1)))
        (when (and beg end)
          (downcase-region beg end))))))

(provide 'eriks-fix-last-shift-mistake)
