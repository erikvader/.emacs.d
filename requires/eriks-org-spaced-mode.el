(defun eriks/spaced--lineize-region (beg end)
  "Returns a cons cell with BEG and END but with BEG extended to the
beginning of its line and END extended to the end of its line."
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
  "Remove all text properties between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(display nil))))

(defun eriks/spaced-org-add-text-properties (beg end)
  "add text properties to all top-level headlines in region between
BEG and END."
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
  "Add spacing to all headlines in the current buffer. If REMOVE-ONLY
is non-nil then remove all added spacing."
  (eriks/spaced-org-remove-text-properties (point-min) (point-max))
  (unless remove-only
    (eriks/spaced-org-add-text-properties (point-min) (point-max))))

(defun eriks/spaced-after-change (beg end asd)
  "Fix text properties linewise. Used in `after-change-functions'"
  (when eriks/org-spaced-mode
    (pcase (eriks/spaced--lineize-region beg end)
      (`(,beg . ,end)
       (eriks/spaced-org-remove-text-properties beg end)
       (eriks/spaced-org-add-text-properties beg end)))))

(define-minor-mode eriks/org-spaced-mode
  "Makes all top level headings have extra virtual space above them
for better readability."
  nil " Spc" nil
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (if eriks/org-spaced-mode
      (progn
        (eriks/spaced-spacify-buffer)
        (add-hook 'after-change-functions #'eriks/spaced-after-change))
    (eriks/spaced-spacify-buffer t)
    (remove-hook 'after-change-functions #'eriks/spaced-after-change)))

(defvar-local eriks/startup--org-spaced-mode nil
  "If non-nil then start `eriks/org-spaced-mode' when entering org-mode buffer.")
(add-to-list 'org-startup-options '("eriks/spaced" eriks/startup--org-spaced-mode t))

(defun eriks/spaced-maybe-start ()
  "Start `eriks/org-spaced-mode' if \"eriks/spaced\" is present in
  +STARTUP."
  (when eriks/startup--org-spaced-mode
    (eriks/org-spaced-mode 1)))

(add-hook 'org-mode-hook #'eriks/spaced-maybe-start)

(provide 'eriks-org-spaced-mode)
