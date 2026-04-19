;; -*- lexical-binding: t; -*-

(defun eriks/widen-marker (&rest _args)
  "This simply marks that this pair widens")

(define-advice sp--unwrap-sexp (:filter-args (args) widen)
  "Remove the extra spaces added from `eriks/widen-marker'."
  (cl-destructuring-bind (sexp &optional no-cleanup) args
    (if (or no-cleanup
            (not (memq 'eriks/widen-marker
                       (plist-get (sp-get-pair (sp-get sexp :op))
                                  :post-handlers))))
        args
      (let ((chars-deleted 0))
        (save-excursion
          (goto-char (sp-get sexp :end-in))
          (when (looking-back " ")
            (delete-char -1)
            (cl-incf chars-deleted))
          (goto-char (sp-get sexp :beg-in))
          (when (looking-at " ")
            (delete-char 1)
            (cl-incf chars-deleted)))
        ;; NOTE: assumes it is fine to modify the original. The docs of
        ;; `sp--unwrap-sexp' says that the sexp structure is invalid afterwards anyways,
        ;; so it is probably fine.
        (list (plist-put sexp :end (- (plist-get sexp :end) chars-deleted))
              no-cleanup)))))

(defun eriks/sp-post-handlers (modes parens &rest what)
  "Install post-handlers for all PARENS in all MODES according to WHAT."
  (let (handlers)
    (when (memq :open what)
      (push '("||\n[i]" "RET") handlers))
    (when (memq :widen what)
      (push '("| " "SPC") handlers)
      (push 'eriks/widen-marker handlers))
    (dolist (i (ensure-list parens))
      (sp-local-pair modes i nil :post-handlers handlers))))

(provide 'eriks-sp-post-handlers)
