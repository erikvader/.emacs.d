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
  "An `evil-shift-right' that keeps visual mode activated.

See the note in `eriks/evil-indent-line-left'."
  :motion evil-line
  :type line
  :keep-visual t
  :move-point nil
  (interactive "<r><c>")
  (let ((tab-width evil-shift-width))
    (indent-rigidly-right-to-tab-stop beg end))
  (setq deactivate-mark nil))

(evil-define-operator eriks/evil-indent-line-left (beg end dir)
  "An `evil-shift-left' that keeps visual mode activated.

It's not actually using `evil-shift-left', but
`indent-rigidly-left-to-tab-stop' instead, and that is because the
former changes the relative indentation between the lines in a
selection, while the latter just moves the lines to the side, as
expected. This means that maybe not all settings related to the former
will take effect here."
  :motion evil-line
  :type line
  :keep-visual t
  :move-point nil
  (interactive "<r><c>")
  (let ((tab-width evil-shift-width))
    (indent-rigidly-left-to-tab-stop beg end))
  (setq deactivate-mark nil))

(provide 'eriks-evil-drag-line)
