(defun eriks/org-remove-done-tasks ()
  "delete all headers marked as DONE on the current subtree"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-mark-subtree)
     (delete-region (region-beginning) (region-end))
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

(defun eriks/org-format ()
  "Tries to neatly format an Org mode file.
What it tries to do:
  - make sure that there is exactly one empty line before any top level heading
  - deletes trailing lines"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (cl-labels ((donext ()
                        (let ((p (re-search-forward "^\\* " nil t)))
                          (when p
                            (save-excursion
                              (forward-line -1)
                              (just-one-line))
                            (donext))))
                (just-one-line ()
                               (end-of-line)
                               (let ((cur (point)))
                                 (skip-chars-backward " \t\n")
                                 (delete-region cur (point)))
                               (newline)))
      (donext)
      (eriks/delete-trailing-lines))))

(add-to-list 'safe-local-eval-forms '(eriks/org-format))

(provide 'eriks-org-utils)
