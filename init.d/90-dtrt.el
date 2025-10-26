;; NOTE: Put this last to make sure it doesn't unnecessarily guess the indentation of
;; buffers during initialization.
(use-package dtrt-indent
  :ensure t
  :diminish ""
  :custom
  (dtrt-indent-verbosity 1)
  ;; NOTE: the definition of `dtrt-indent-global-mode' tries to specify that it should
  ;; only be enabled in prog-mode and text-mode, but there is an extra t there, which
  ;; makes it enabled everywhere. This is for some reason breaking python-eldoc.
  (dtrt-indent-global-modes '(prog-mode text-mode))
  :config
  (dtrt-indent-global-mode))
