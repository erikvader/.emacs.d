(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-mode-line-color nil)
  :config
  (define-advice flycheck-mode-line-status-text (:filter-return (text) colorize)
    "Colorize the modeline differently."
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
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)

  (flycheck-add-next-checker 'python-pylint '(warning . python-pyright))
  (global-flycheck-eglot-mode 1)

  ;; NOTE: I don't want special faces for unused stuff, an underline is enough.
  (face-spec-set 'flycheck-deprecated nil 'face-defface-spec)
  (face-spec-set 'flycheck-unnecessary nil 'face-defface-spec)

  ;; NOTE: Originally taken from
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun eriks/flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK.

Flycheck can display in eldoc by itself as of version 37, but it doesn't
work like I want it to. For the first, it display multiline errors in
the echo area, I prefer it being as small as possible, so this custom
function is only showing a single line in the echo area per error. The
second reason is that flycheck seems to have some weird bug where it
shows the whole `eldoc-doc-buffer' by itself, which is way more
distracting than a multiline echo. This is probably just a temporary bug
that will get fixed though."
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
    (when-let* (((not (bound-and-true-p lsp-mode)))
                ((not (flycheck-eglot--enabled-p)))
                ((not flycheck-mode))
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
  :gfhook
  ;; HACK: evil fixes the keymaps frequently, e.g. when switching states, but apparently
  ;; not the moment when flycheck is activated, so add an extra normalize here.
  (nil 'evil-normalize-keymaps)
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
                       (eriks/flycheck-activate-if-started-projectile)))
  :general-config
  ('flycheck-error-list-mode-map
   ;; TODO: it would be nice if there were more navigation commands, like goto parent
   ;; section or something.
   "k" #'flycheck-error-list-previous-error
   ;; TODO: next and prev doesn't make sure the whole diagnostic is visible on screen. The
   ;; rust-analyzer likes to output multiline ones for example.
   "j" #'flycheck-error-list-next-error
   "r" #'flycheck-error-list-visit-related-location
   "1" #'flycheck-error-list-group-by-none
   "2" #'flycheck-error-list-group-by-file
   "3" #'flycheck-error-list-group-by-checker
   "4" #'flycheck-error-list-group-by-level))

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
