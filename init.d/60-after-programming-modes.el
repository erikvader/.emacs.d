(use-package highlight-indent-guides
  :defer t
  :ensure t
  :diminish
  :custom
  (highlight-indent-guides-auto-character-face-perc 40)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-auto-top-character-face-perc 70)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :ghook 'python-mode-hook)

(use-package flycheck
  :ensure t
  :config
  ;; (flycheck-select-checker 'haskell-hlint)
  :ghook
  'c-mode-hook
  'c++-mode-hook
  'python-mode-hook
  'rust-mode-hook
  'sh-mode-hook
  'haskell-mode
  'LaTeX-mode-hook)
