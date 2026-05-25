(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-mode-line-color nil)
  :config
  (define-advice flycheck-mode-line-status-text (:filter-return (text) colorize)
    (cond ((string-match "^\\(.*\\):\\([0-9]+\\)|\\([0-9]+\\)|\\([0-9]+\\)" text)
           `(,(match-string 1 text)
             ":" (:propertize ,(match-string 2 text) face error)
             "|" (:propertize ,(match-string 3 text) face warning)
             "|" (:propertize ,(match-string 4 text) face success)))
          ((string-match "^\\(.*\\):\\([0-9]+\\)" text)
           `(,(match-string 1 text)
             ":" (:propertize ,(match-string 2 text) face success)))
          (t text)))
  (general-define-key :keymaps 'flycheck-mode-map flycheck-keymap-prefix nil)
  (eriks/leader-def 'normal 'flycheck-mode-map
    "f" flycheck-command-map)
  (evil-collection-flycheck-setup)
  (flycheck-add-next-checker 'python-pylint '(warning . python-pyright))

  ;; NOTE: Originally taken from
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun eriks/flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :echo (-> (flycheck-error-message err)
                            (split-string "\n")
                            car)
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  :init
  (cl-defun eriks/flycheck-activate-if-started-projectile ()
    "Activates `flycheck-mode' in the current buffer if another buffer
in the same projectile project also has flycheck enabled."
    (when-let (((not (bound-and-true-p lsp-mode)))
               ((flycheck-may-enable-mode))
               (root (projectile-project-root))
               (cur-mode major-mode)
               ((cl-some (lambda (buf)
                           (with-current-buffer buf
                             (and (buffer-file-name buf)
                                  (eq major-mode cur-mode)
                                  flycheck-mode)))
                         (projectile-project-buffers root))))
      (flycheck-mode 1)))
  :general-config
  ('flycheck-command-map
   "f" 'flycheck-first-error)
  :gfhook
  (nil (cl-defun eriks/flycheck-prefer-eldoc ()
         "Display flycheck text using eldoc to prevent minibuffer conflicts."
         (add-hook 'eldoc-documentation-functions #'eriks/flycheck-eldoc nil t)
         (setq-local flycheck-display-errors-function nil
                     flycheck-help-echo-function nil)))
  ('(sh-mode-hook LaTeX-mode-hook minizinc-mode-hook)
   'flycheck-mode-on-safe)
  ('haskell-mode-hook (cl-defun eriks/flycheck-haskell-hook ()
                        (setq-local flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc))
                        (setq-local flycheck-checker 'haskell-hlint)
                        (eriks/flycheck-activate-if-started-projectile)))
  ('python-mode-hook (cl-defun eriks/flycheck-python-hook ()
                       ;; NOTE: find a uv project with a venv and make flycheck use the
                       ;; executables in that venv
                       (when-let* ((venv-bin (eriks/python-find-venv-bin)))
                         (setq-local flycheck-python-pylint-executable (file-name-concat venv-bin "pylint")
                                     flycheck-python-pyright-executable (file-name-concat venv-bin "pyright")
                                     flycheck-python-mypy-executable (file-name-concat venv-bin "mypy")))

                       ;; NOTE: a temp file is created each time some checker, I have
                       ;; forgotten, is run, so this reduces the number of times it is
                       ;; invoked.
                       (setq-local flycheck-check-syntax-automatically '(save mode-enable))
                       (setq-local flycheck-checker 'python-pylint)
                       ;; NOTE: pylint can find its own configuration files, no need for
                       ;; flycheck to find them first using its own rules
                       (setq-local flycheck-pylintrc nil
                                   flycheck-python-mypy-config nil)
                       ;; NOTE: I don't like types in python and find it unnecessary to
                       ;; run this automatically after pylint
                       ;; (setq-local flycheck-disabled-checkers '(python-mypy))
                       (eriks/flycheck-activate-if-started-projectile))))

(use-package flymake
  :disabled ;; TODO: i used this for eglot, but not anymore, so remove?
  :custom
  (flymake-fringe-indicator-position nil)
  (flymake-margin-indicator-position nil)
  :config
  (evil-collection-flymake-setup)
  (eriks/leader-def 'normal 'flymake-mode-map
    :infix "f"
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error
    "l" 'flymake-show-project-diagnostics
    "b" 'flymake-show-buffer-diagnostics))

(use-package apheleia
  :ensure t
  :diminish "Aph"
  :config
  (add-to-list 'apheleia-formatters
               ;; NOTE: copy of the default black config with uv run added
               '(black-uv "uv" "run" "--only-dev" "black"
                          (when (apheleia-formatters-extension-p "pyi") "--pyi")
                          (apheleia-formatters-fill-column "--line-length")
                          "--stdin-filename" filepath "-"))
  :gfhook
  ('python-mode-hook (cl-defun eriks/apheleia-uv-black-python ()
                       (when-let* ((venv-bin (eriks/python-find-venv-bin))
                                   ((file-regular-p (file-name-concat venv-bin "black"))))
                         (setq-local apheleia-formatter 'black-uv
                                     ;; NOTE: black uses this maximum line length by
                                     ;; default
                                     fill-column 88))))
  :ghook 'emacs-lisp-mode-hook)

(use-package rainbow-delimiters
  :ensure t
  :ghook
  'prog-mode-hook
  'conf-mode-hook
  'TeX-mode-hook)

;; TODO: add ansi-color-compilation-filter till compilation-filter-hook for colors, but do
;; i really need that? There is also some variable to adjust the environment to change
;; TERM.
(use-package compile
  :custom
  (compilation-scroll-output t)
  :config
  (evil-collection-compile-setup)
  (eriks/leader-def 'normal
    "c" 'compile
    "r" 'recompile)
  :gfhook
  ('rust-mode-hook (cl-defun eriks/rust-compile-hook ()
                     (setq-local compile-command "cargo build "))))
