;; -*- lexical-binding: t; -*-

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-stay-out-of '(yasnippet company))
  :gfhook
  ;; NOTE: evil fixes the keymaps frequently, e.g. when switching states, but apparently
  ;; not the moment when eglot is activated, so add an extra normalize here.
  ('eglot-managed-mode-hook `(evil-normalize-keymaps
                              ,(cl-defun eriks/disable-inlay-hints-mode ()
                                 "I don't want to see this by default"
                                 (eglot-inlay-hints-mode -1))))
  :config
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  ( ;; make emacs indent this correctly
                   :check (:command "clippy")
                   ;; TODO: use this?
                   ;; :inlayHints (:closingBraceHints (:enable :json-false))
                   ))))

  ;; TODO: move these formatting thingies to a file
  (progn
    (defun eriks/eglot--format-update ()
      "Update the state of formatting on save in the current buffer."
      (if (and eriks/eglot-format-on-save-mode
               (eglot-managed-p))
          ;; TODO: use apheleia if it is slow/bad to use before-save-hook
          ;; https://github.com/radian-software/apheleia/issues/153#issuecomment-1452783713
          ;; https://github.com/doomemacs/doomemacs/blob/d2d805a42b141a03c17e5e957bb0b236d691b971/modules/editor/format/autoload/lsp.el#L75
          (add-hook 'before-save-hook #'eglot-format-buffer nil t)
        (remove-hook 'before-save-hook #'eglot-format-buffer t)))

    (defun eriks/eglot--format-lighter ()
      "Return the lighter string"
      (if (memq #'eglot-format-buffer before-save-hook) " Fmt" ""))

    (define-minor-mode eriks/eglot-format-on-save-mode
      "Format an eglot managed buffer when saving."
      :lighter (:eval (eriks/eglot--format-lighter))
      (if eriks/eglot-format-on-save-mode
          (add-hook 'eglot-managed-mode-hook #'eriks/eglot--format-update nil t)
        (remove-hook 'eglot-managed-mode-hook #'eriks/eglot--format-update t))
      (eriks/eglot--format-update)))

  ;; TODO: put in its own file?
  (progn
    (defun eriks/eglot-rust-hover-hook ()
      "Installs a modified eldoc documention function that works better for rust."
      (if (and (eglot-managed-p)
               (derived-mode-p 'rust-mode))
          (progn
            (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
            (add-hook 'eldoc-documentation-functions #'eriks/eglot-rust-hover-eldoc-function nil t))
        (remove-hook 'eldoc-documentation-functions #'eriks/eglot-rust-hover-eldoc-function t)))

    (add-hook 'eglot-managed-mode-hook #'eriks/eglot-rust-hover-hook)

    (defun eriks/eglot-rust-hover-eldoc-function (cb)
      "This wraps the normal eglot eldoc hover function to find the actual
function signature in the hover text. Eglot normally displays the module
path in the echo area since that is what is on the first line."
      (eglot-hover-eldoc-function (lambda (info &rest args)
                                    (let ((echo (pcase (seq-remove #'string-blank-p (string-lines info))
                                                  ((and `(,_ ,fn-line . ,_)
                                                        (guard (string-search "fn " fn-line)))
                                                   fn-line)
                                                  (`(,module-line . ,_)
                                                   module-line)
                                                  (_ nil))))
                                      (apply cb info :echo echo args))))))

  (eriks/leader-def 'normal 'eglot-mode-map
    :infix "l"
    "h" 'eglot-inlay-hints-mode
    "a" 'eglot-code-actions
    "o" 'eglot-code-action-organize-imports
    "q" 'eglot-code-action-quickfix
    "e" 'eglot-code-action-extract
    "i" 'eglot-code-action-inline
    "w" 'eglot-code-action-rewrite
    "f" 'eglot-format
    "r" 'eglot-rename
    "C-c" 'eglot-shutdown))

(use-package eglot-x
  :vc (:url https://github.com/nemethf/eglot-x.git :rev :newest)
  :custom
  (eglot-x-enable-encoding-negotiation nil) ;; NOTE: this is not needed in newer versions
  (eglot-x-enable-hover-actions nil) ;; NOTE: no clickable actions in hover
  (eglot-x-enable-snippet-text-edit nil) ;; NOTE: no snippets please
  :general-config
  ('eglot-mode-map
   [remap pp-macroexpand-last-sexp] 'eglot-x-expand-macro
   [remap xref-find-references] 'eglot-x-find-refs)
  :config
  (eriks/leader-def 'normal 'eglot-mode-map
    :infix "l"
    "t" 'eglot-x-ask-runnables
    "g" 'eglot-x-find-workspace-symbol
    "s" 'eglot-x-structural-search-replace
    "j" 'eglot-x-join-lines
    "C-w" 'eglot-x-reload-workspace
    "C-p" 'eglot-x-rebuild-proc-macros
    "c" 'eglot-x-find-crate
    "C" 'eglot-x-view-crate-graph)

  (eriks/defkey-repeat eglot-x-drag-stuff
    :prefix eriks/leader
    :infix "l"
    :modes 'normal
    :keymaps 'eglot-mode-map
    "M-k" 'eglot-x-move-item-up
    "M-j" 'eglot-x-move-item-down)

  (eriks/leader-def 'normal 'eglot-mode-map
    :infix "o"
    "d" 'eglot-x-open-external-documentation)

  (eglot-x-setup))

;; TODO: remove? This doesn't work that well
(use-package flycheck-eglot
  :disabled
  :ensure t
  :custom
  (flycheck-eglot-enable-diagnostic-tags nil)
  :config
  (global-flycheck-eglot-mode 1)
  :gfhook
  ;; NOTE: this is recommended by the author for rust-analyzer
  ('rust-mode-hook 'flycheck-eglot-disable-diagnostics-pull))
