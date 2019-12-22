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

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting nil) ;TODO: change face
  :gfhook
  ('rust-mode-hook 'lsp) ;; rustup component add rls rust-analysis rust-src
  ('python-mode-hook 'lsp) ;; pacman -S python-language-server python-pyflakes python-rope flake8 python-pycodestyle yapf python-pydocstyle
  )

(use-package lsp-pyls
  :custom
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  (lsp-pyls-plugins-pylint-args (vector (concat "--rcfile=" user-emacs-directory ".flycheck-pylintrc"))))

(use-package lsp-ui
  :ensure t
  :after (:and lsp-mode flycheck)
  :ghook 'lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-delay 1)
  :general
  ('normal
   :prefix eriks/leader
   "d" 'lsp-ui-doc-glance))

(use-package company-lsp
  :ensure t
  :after (:and company lsp-mode)
  :custom
  (company-lsp-enable-snippet nil)
  :config
  (push 'company-lsp company-backends)
   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

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
  'haskell-mode)

