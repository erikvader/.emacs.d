(use-package lsp-mode
  :ensure t
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  :config
  (eriks/leader-def
    'normal
    'lsp-mode-map
    "l" 'lsp-command-map)

  (defun lsp-toggle-highlighting ()
    "Toggles `lsp-enable-symbol-highlighting'.

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

  ;; TODO: replaced by `lsp-format-buffer-on-save'?
  (defun eriks/lsp-install-format-on-save-hooks-toggle (&optional arg)
    "Toggle whether `lsp-format-buffer' should be run before
save. Turn on if ARG is positive, toggle if 0 and turn off if
negative."
    (interactive "P")
    (let ((arg (if (null arg)
                   0
                 (prefix-numeric-value arg))))
      (cond ((and (<= arg 0)
                  (local-variable-p 'before-save-hook)
                  (memq #'lsp-format-buffer before-save-hook))
             (remove-hook 'before-save-hook #'lsp-format-buffer t)
             (when (interactive-p)
               (message "no longer formatting on save")))
            ((and (>= arg 0)
                  (null (memq #'lsp-format-buffer before-save-hook)))
             ;;TODO: does order with `lsp--before-save' matter?
             (add-hook 'before-save-hook #'lsp-format-buffer nil t)
             (when (interactive-p)
               (message "formatting on save enabled")))
            ((interactive-p)
             (message "formatting on save already on/off")))))

  (defvar-local eriks/lsp-format-on-save nil
    "Whether `lsp-format-buffer' should run on save")
  (put 'eriks/lsp-format-on-save 'safe-local-variable #'booleanp)
  (defun eriks/lsp-format-on-save-hook ()
    (if (bound-and-true-p eriks/lsp-format-on-save)
        (eriks/lsp-install-format-on-save-hooks-toggle 1)
      ;; If this hook is run before local variables are set, make sure it is run after.
      ;; https://stackoverflow.com/a/5148435
      (add-hook 'hack-local-variables-hook #'eriks/lsp-format-on-save-hook nil t)))
  :gfhook
  ('lsp-mode-hook 'eriks/lsp-format-on-save-hook))

;; TODO: (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;TODO: (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(use-package lsp-ui
  :ensure t
  :ghook 'lsp-mode
  :custom
  (lsp-ui-doc-enable nil) ;; TODO: hur få fram en skrollbar doc?
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-delay 1))
;; TODO: funkar imenu?

(use-package lsp-ui-sideline
  :gfhook
  ('flycheck-mode-hook 'lsp-ui-sideline-mode))

(use-package lsp-pyls
  :custom
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  :gfhook
  ('python-mode-hook 'eriks/lsp-if-already-started))

(use-package lsp-rust
  :custom
  (lsp-rust-clippy-preference "on")
  (lsp-rust-analyzer-closing-brace-hints nil)
  (lsp-rust-analyzer-call-info-full nil)
  :gfhook
  ('rust-mode-hook 'eriks/lsp-if-already-started))

(use-package ccls
  :ensure t
  :gfhook
  ('(c-mode-hook c++-mode-hook java-mode-hook) 'eriks/lsp-if-already-started))

(use-package lsp-java
  :ensure t)

;; TODO: https://github.com/emacs-lsp/lsp-ivy??
