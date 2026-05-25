(use-package lsp-mode
  :disabled
  :ensure t
  :custom
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-completion-provider :none)
  (lsp-format-buffer-on-save t)
  (lsp-keymap-prefix nil)
  (lsp-eldoc-render-all nil)
  (lsp-inlay-hint-enable t)
  (lsp-modeline-code-action-icons-enable nil) ;; TODO: did this remove the icon?
  :config
  (eriks/leader-def
    'normal
    'lsp-mode-map
    "l" lsp-command-map)

  (defun lsp-toggle-highlighting ()
    "Toggles `lsp-enable-symbol-highlighting'.

source: https://github.com/emacs-lsp/lsp-mode/issues/515#issuecomment-564665576"
    (interactive)
    (setq lsp-enable-symbol-highlighting (not lsp-enable-symbol-highlighting))
    ;; TODO: find a better way to make lsp mode realise the changed setting
    (cond
     ((and lsp-enable-symbol-highlighting  (lsp--capability "documentHighlightProvider"))
      (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t)
      (lsp--info "Highlighting enabled."))
     ((not lsp-enable-symbol-highlighting)
      (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)
      (lsp--remove-overlays 'lsp-highlight)
      (lsp--info "Highlighting disabled."))
     (t (user-error "Current server does not support highlights?"))))

  ;; TODO: native solution?
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
          (lsp))))))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil))

(use-package lsp-pyls
  :disabled
  :custom
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  :gfhook
  ('python-mode-hook 'eriks/lsp-if-already-started))

;; TODO: open external docs, bind it
(use-package lsp-rust
  :disabled
  :custom
  (lsp-rust-clippy-preference "on")
  (lsp-rust-analyzer-closing-brace-hints nil)
  :gfhook
  ('rust-mode-hook 'eriks/lsp-if-already-started)
  :gfhook
  ('lsp-mode-map
   [remap pp-macroexpand-last-sexp] #'lsp-rust-analyzer-expand-macro))

(use-package ccls
  :disabled
  :ensure t
  :gfhook
  ('(c-mode-hook c++-mode-hook java-mode-hook) 'eriks/lsp-if-already-started))

(use-package lsp-java
  :disabled
  :ensure t)

;; TODO: bind `lsp-ivy-workspace-symbol' to something
(use-package lsp-ivy
  :disabled
  :ensure t)
