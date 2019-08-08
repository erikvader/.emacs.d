(defun eriks/spaced--lineize-region (beg end)
  (let (new-beg
        new-end)
    (save-excursion
      (goto-char beg)
      (setq new-beg (line-beginning-position)))
    (save-excursion
      (goto-char end)
      (setq new-end (line-end-position)))
    (cons new-beg new-end)))

(defun eriks/spaced-org-remove-text-properties (beg end)
  (with-silent-modifications
    (remove-text-properties beg end '(display nil))))

(defun eriks/spaced-org-add-text-properties (beg end)
  (save-match-data
    (save-excursion
      (with-silent-modifications
        (goto-char beg)
        (let ((found-more t))
          (while (and found-more
                      (<= (point) end)
                      (not (eobp)))
            (setq found-more (search-forward-regexp "^\\*\\( \\)" end t))
            (when found-more
              (add-text-properties (match-beginning 1)
                                   (match-end 1)
                                   '(display (space . (:relative-height 1.8)))))))))))

(defun eriks/spaced-spacify-buffer (&optional remove-only)
  (interactive "P")
  (eriks/spaced-org-remove-text-properties (point-min) (point-max))
  (unless remove-only
    (eriks/spaced-org-add-text-properties (point-min) (point-max))))

(defun eriks/spaced-after-change (beg end asd)
  (when eriks/org-spaced-mode
    (pcase (eriks/spaced--lineize-region beg end)
      (`(,beg . ,end)
       (eriks/spaced-org-remove-text-properties beg end)
       (eriks/spaced-org-add-text-properties beg end)))))

(define-minor-mode eriks/org-spaced-mode
  "Makes all top level headings have extra space above them."
  nil " Spc" nil
  (if eriks/org-spaced-mode
      (progn
        (eriks/spaced-spacify-buffer)
        (add-hook 'after-change-functions #'eriks/spaced-after-change))
    (eriks/spaced-spacify-buffer t)
    (remove-hook 'after-change-functions #'eriks/spaced-after-change)))

(provide 'eriks-org-spaced-mode)
