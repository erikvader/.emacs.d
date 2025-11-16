(evil-define-motion eriks/sp-jump-item ()
  "Like `evil-jump-item', but with smartparens definitions.

This jumps between be start of each opening or closing pair. It doesn't
work if point is anywhere else than the start, so multi-character pairs
don't work so well with this motion."
  :jump t
  :type inclusive
  (let ((use (or (sp-get-sexp)
                 (user-error "No sexp found"))))
    (sp-get use
      (goto-char (if (>= (point) :end-in)
                     :beg
                   :end-in)))))

(provide 'eriks-sp-jump-item)
