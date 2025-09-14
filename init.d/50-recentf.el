(use-package recentf
  :config
  (recentf-mode 1)
  (defun recentf-save-list-silent ()
    (let ((inhibit-message t))
      (recentf-save-list)))
  (run-at-time nil (* 5 60) 'recentf-save-list-silent)
  (add-to-list 'recentf-exclude "recentf")
  :general-config
  ("C-x C-r" 'recentf-open)
  :custom
  (recentf-max-saved-items 100))
