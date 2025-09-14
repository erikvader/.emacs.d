(use-package help-mode
  :gfhook 'scroll-lock-mode
  :config
  (evil-collection-help-setup)
  (add-to-list 'popper-reference-buffers 'help-mode)

  (defun eriks/help-revert-with-characters ()
    "Revert the current help buffer, but print all values with characters
instead of integers."
    (interactive)
    (let ((print-integers-as-characters t))
      (revert-buffer))
    (message "Using characters instead of integers"))

  (defun eriks/help-revert-with-circle ()
    "Revert the current help buffer, but with the value of `print-circle'
set to nil. The default can make the output of, for example `sp-pairs',
difficult to read."
    (interactive)
    (cl-letf* ((org-prin1 (symbol-function 'cl-prin1-to-string))
               ((symbol-function 'cl-prin1-to-string) (lambda (&rest args2)
                                                        (let ((print-circle nil))
                                                          (apply org-prin1 args2)))))
      (revert-buffer))
    (message "Printing recursive structures as is"))
  :general-config
  ('normal
   'help-mode-map
   "p" 'eriks/help-revert-with-characters
   "o" 'eriks/help-revert-with-circle))
