(use-package lsp-mode
  :ensure t
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-enable nil)
  :general
  ('normal
   'lsp-mode-map
   :prefix eriks/leader
   "a" 'lsp-execute-code-action)
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

  (defun eriks/lsp-if-already-started ()
    "Runs `lsp' only if it would connect to an already running
server on some workspace."
    (interactive)
    (when (buffer-file-name)
      (let* ((session (lsp-session))
             (sess-folder (lsp-find-session-folder session (buffer-file-name)))
             (project-root (and sess-folder
                                (lsp-canonical-file-name sess-folder)))
             (clients (lsp--find-clients)))
        (when (and project-root
                   (seq-some (lambda (client)
                               ;;TODO: (lsp--find-multiroot-workspace session client project-root)
                               ;; this wanted?? What is a multiroot workspace?
                               (lsp--find-workspace session client project-root))
                             clients))
          (lsp)))))

  (defun eriks/lsp-install-format-on-save-hooks-toggle ()
    "Toggle whether `lsp-format-buffer' should be run before save."
    (interactive)
    (if (and (local-variable-p 'before-save-hook)
             (memq #'lsp-format-buffer before-save-hook))
        (progn
          (remove-hook 'before-save-hook #'lsp-format-buffer t)
          (message "no longer formatting on save"))
      ;TODO: does order with `lsp--before-save' matter?
      (add-hook 'before-save-hook #'lsp-format-buffer nil t)
      (message "formatting on save enabled"))))

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
   'lsp-ui-mode-map
   :prefix eriks/leader
   "d" 'lsp-ui-doc-glance))

(use-package lsp-ui-sideline
  :after flycheck
  :gfhook
  ('flycheck-mode-hook 'lsp-ui-sideline-mode))

(use-package lsp-pyls
  :custom
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  :gfhook
  ;; pacman -S python-language-server python-pyflakes python-pylint
  ('python-mode-hook 'eriks/lsp-if-already-started))

(use-package lsp-rust
  :custom
  (lsp-rust-clippy-preference "on")
  :gfhook
  ;; rustup component add rls rust-analysis rust-src
  ('rust-mode-hook 'eriks/lsp-if-already-started))

(use-package ccls
  :ensure t
  ;; :custom
  ;; (ccls-sem-highlight-method 'font-lock)
  :gfhook
  ;; pacman -S ccls
  ('(c-mode-hook c++-mode-hook) 'eriks/lsp-if-already-started))

(use-package lsp-java
  :ensure t)
