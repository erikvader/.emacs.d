;; NOTE: Put this last to make sure it doesn't unnecessarily guess the indentation of
;; buffers during initialization.
(use-package dtrt-indent
  :ensure t
  :diminish ""
  :custom
  ;; NOTE: It was the fact that it tried to show this message at the same time as
  ;; python-eldoc that broke it, so turning it off also solves the problem.
  ;; The variable `dtrt-indent-original-indent' can be inspected to see what dtrt changed.
  (dtrt-indent-verbosity 0)
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list '(python-mode default python-indent-offset))
  :gfhook
  ('eriks/editable-file-hook #'dtrt-indent-mode))
