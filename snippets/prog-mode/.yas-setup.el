(defun eriks/yas-comment-end()
  (unless (eq (length comment-end) 0)
    (concat " " (yas-trimmed-comment-end))))
