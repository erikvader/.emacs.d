(use-package lsp-mode
  :ensure t
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-keep-workspace-alive nil)
  :gfhook
  ('rust-mode-hook 'eriks/lsp-if-already-started) ;; rustup component add rls rust-analysis rust-src
  ('python-mode-hook 'eriks/lsp-if-already-started) ;; pacman -S python-language-server python-pyflakes python-pylint
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

  (defun eriks/lsp-hover-or-signature-help ()
    "Do the hover action thingy when in non-insert and the signature help
    thingy when in insert.
source: https://github.com/emacs-lsp/lsp-mode/issues/214#discussion_r242426774"
    (if (evil-insert-state-p)
        (lsp-signature)
      (lsp-hover)))
  (remove-hook 'lsp-eldoc-hook 'lsp-hover)
  (add-hook 'lsp-eldoc-hook 'eriks/lsp-hover-or-signature-help)

  (defun eriks/lsp-if-already-started ()
    "Runs `lsp' only if it would connect to an already running
server on some workspace."
    (interactive)
    (when (buffer-file-name)
      (let* ((session (lsp-session))
             (sess-folder (lsp-find-session-folder session (buffer-file-name)))
             (project-root (and sess-folder
                                (lsp-cannonical-file-name sess-folder)))
             (clients (lsp--find-clients)))
        (when (and project-root
                   (seq-some (lambda (client)
                               ;;TODO: (lsp--find-multiroot-workspace session client project-root)
                               ;; this wanted?? What is a multiroot workspace?
                               (lsp--find-workspace session client project-root))
                             clients))
          (lsp))))))

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
   'lsp-ui-mode-map
   :prefix eriks/leader
   "d" 'lsp-ui-doc-glance))

(use-package lsp-ui-sideline
  :after flycheck
  :gfhook
  ('flycheck-mode-hook 'lsp-ui-sideline-mode))

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
