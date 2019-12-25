(use-package lsp-mode
  :ensure t
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting nil)
  :gfhook
  ('rust-mode-hook 'lsp) ;; rustup component add rls rust-analysis rust-src
  ('python-mode-hook 'lsp) ;; pacman -S python-language-server python-pyflakes python-rope flake8 python-pycodestyle yapf python-pydocstyle
  :config
  (defun lsp-toggle-highlighting ()
    "Toggles highlight.
source: https://github.com/emacs-lsp/lsp-mode/issues/515#issuecomment-564665576"
    (interactive)
    (setq lsp-enable-symbol-highlighting (not lsp-enable-symbol-highlighting))
    (cond
     ((and lsp-enable-symbol-highlighting  (lsp--capability "documentHighlightProvider"))
      (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t)
      (lsp--info "Highlighting enabled."))
     ((not lsp-enable-symbol-highlighting)
      (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)
      (lsp--remove-overlays 'lsp-highlight)
      (lsp--info "Highlighting disabled."))
     (t (user-error "Current server does not support highlights?"))))

  (defun eriks/lsp-hover-or-signature-help ()
    "Do the hover action thingy when in non-insert and the signature help
    thingy when in insert.
source: https://github.com/emacs-lsp/lsp-mode/issues/214#discussion_r242426774"
    (if (evil-insert-state-p)
        (lsp-signature)
      (lsp-hover)))
  (remove-hook 'lsp-eldoc-hook 'lsp-hover)
  (add-hook 'lsp-eldoc-hook 'eriks/lsp-hover-or-signature-help))

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
