;; NOTE: Put this last to make sure it doesn't unnecessarily guess the indentation of
;; buffers during initialization.
(use-package dtrt-indent
  :ensure t
  :custom
  (dtrt-indent-lighter " dtrt")
  ;; NOTE: It was the fact that it tried to show this message at the same time as
  ;; python-eldoc that broke it, so turning it off solves the problem.
  ;; The variable `dtrt-indent-original-indent' can be inspected to see what dtrt changed.
  (dtrt-indent-verbosity 0)
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list '(python-mode default python-indent-offset))
  (defun eriks/dtrt-redo-verbosely ()
    "Reactivate dtrt while being verbose."
    (interactive)
    (when dtrt-indent-mode
      (dtrt-indent-mode -1))
    (let ((dtrt-indent-verbosity 2))
      (dtrt-indent-mode 1)))
  :gfhook
  ('eriks/editable-file-hook #'dtrt-indent-mode))
