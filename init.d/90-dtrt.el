;; Put this last to make sure it doesn't unnecessarily guess the indentation of buffers

;; python-mode already does this by itself `python-indent-guess-indent-offset'
(use-package dtrt-indent
  :ensure t
  :diminish
  :config
  (dtrt-indent-global-mode))
