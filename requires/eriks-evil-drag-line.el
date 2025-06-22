(evil-define-operator eriks/evil-drag-line-right (beg end dir)
  "Drag the current line one space to the right."
  :motion evil-line
  :type line
  :keep-visual t
  :move-point nil
  (interactive "<r><c>")
  (indent-rigidly-right beg end)
  (setq deactivate-mark nil))

(evil-define-operator eriks/evil-drag-line-left (beg end dir)
  "Drag the current line one space to the left."
  :motion evil-line
  :type line
  :keep-visual t
  :move-point nil
  (interactive "<r><c>")
  (indent-rigidly-left beg end)
  (setq deactivate-mark nil))

(evil-define-operator eriks/evil-indent-line-right (beg end dir)
  "Drag the current line one space to the right."
  :motion evil-line
  :type line
  :keep-visual t
  :move-point nil
  (interactive "<r><c>")
  (save-excursion
    (evil-shift-right beg end))
  (setq deactivate-mark nil))

(evil-define-operator eriks/evil-indent-line-left (beg end dir)
  "Drag the current line one space to the left."
  :motion evil-line
  :type line
  :keep-visual t
  :move-point nil
  (interactive "<r><c>")
  (save-excursion
    (evil-shift-left beg end))
  (setq deactivate-mark nil))

(provide 'eriks-evil-drag-line)
