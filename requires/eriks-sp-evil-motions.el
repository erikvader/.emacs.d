;; NOTE: a hybrid variant of this is not so useful since it doesn't jump back to the same
;; place it came from. It's better to just use the forward and backward motions instead,
;; they are more predictable.
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

(evil-define-motion eriks/sp-evil-end-of-hybrid-sexp ()
  "Move to end of hybrid sexp"
  :jump t
  :type exclusive
  (let ((use (or (sp-get-hybrid-sexp)
                 (user-error "No hybrid sexp found"))))
    (sp-get use
      (goto-char :end-suf))))

(evil-define-motion eriks/sp-evil-beg-of-hybrid-sexp ()
  "Move to beginning of hybrid sexp"
  :jump t
  :type exclusive
  (let ((use (or (sp-get-hybrid-sexp)
                 (user-error "No hybrid sexp found"))))
    (sp-get use
      (goto-char :beg-prf))))

(defun eriks/sp-get-long-hybrid-sexp ()
  "Get the bounds of a long hybrid-sexp.

Like the normal one, but it follows as many lines as possible."
  (cl-flet ((move (func)
              (save-excursion
                (let ((cur-pos (point)))
                  (catch 'break
                    ;; NOTE: it seems like these can't return nil
                    (while-let ((pos (funcall func)))
                      (when (eq pos cur-pos)
                        (throw 'break pos))
                      (setq cur-pos (goto-char pos))))))))
    ;; NOTE: mimics `sp-get-hybrid-sexp'
    (let ((end (move 'sp--get-hybrid-sexp-end)))
      (list :beg (move 'sp--get-hybrid-sexp-beg)
            :end end
            :op ""
            :cl ""
            :prefix ""
            :suffix (sp--get-hybrid-suffix end)))))

(evil-define-motion eriks/sp-evil-beg-of-long-hybrid-sexp ()
  "Move to beginning of long hybrid sexp.

It will include as much as possible."
  :jump t
  :type exclusive
  (let ((use (or (eriks/sp-get-long-hybrid-sexp)
                 (user-error "No hybrid sexp found"))))
    (sp-get use
      (goto-char :beg-prf))))

(evil-define-motion eriks/sp-evil-end-of-long-hybrid-sexp ()
  "Move to end of long hybrid sexp.

It will include as much as possible."
  :jump t
  :type exclusive
  (let ((use (or (eriks/sp-get-long-hybrid-sexp)
                 (user-error "No hybrid sexp found"))))
    (sp-get use
      (goto-char :end-suf))))

;; TODO: should there be an outer version that includes surrounding whitespace or smth?
(evil-define-text-object eriks/evil-sp-inner-hybrid-sexp (count &optional beg end type)
  "Text object for the hybrid sexp, according to smartparens"
  (let ((bounds (or (sp-get-hybrid-sexp)
                    (user-error "Not inside a hybrid sexp"))))
      (sp-get bounds
      (evil-range :beg-prf :end-suf))))

;; TODO: should there be an outer version that includes surrounding whitespace or smth?
(evil-define-text-object eriks/evil-sp-inner-long-hybrid-sexp (count &optional beg end type)
  "Text object for the long hybrid sexp, according to smartparens"
  (let ((bounds (or (eriks/sp-get-long-hybrid-sexp)
                    (user-error "Not inside a hybrid sexp"))))
    (sp-get bounds
      (evil-range :beg-prf :end-suf))))

(evil-define-text-object eriks/evil-sp-a-sexp (count &optional beg end type)
  "Same as `eriks/evil-sp-inner-sexp', but also includes the delimiters,
prefix and suffix."
  (let ((bounds (or (sp-get-enclosing-sexp)
                    (user-error "Not inside an sexp"))))
      (sp-get bounds
      (evil-range :beg-prf :end-suf))))

(evil-define-text-object eriks/evil-sp-inner-sexp (count &optional beg end type)
  "Text object for the inner parts of the enclosing delimiters, according
to smartparens"
  (let ((bounds (or (sp-get-enclosing-sexp)
                    (user-error "Not inside an sexp"))))
      (sp-get bounds
      (evil-range :beg-in :end-in))))

(provide 'eriks-sp-evil-motions)
