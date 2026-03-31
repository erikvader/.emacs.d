(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  (global-eldoc-mode 1)

  (defalias 'eriks/eldoc-doc-buffer #'eldoc-doc-buffer
    "An alias that doesn't select the window when using popper.")
  (advice-add 'eriks/eldoc-doc-buffer :around #'eriks/popper-no-select-advice)

  :general-config
  ('global
   :prefix "C-h"
   "." 'eldoc
   "C-." 'eriks/eldoc-doc-buffer))

(use-package projectile
  :ensure t
  :diminish
  :custom
  (projectile-mode-line-prefix "")
  (projectile-mode-line-function (cl-defun eriks/projectile-mode-line-function ()
                                   "Modified from `projectile-default-mode-line'"
                                   (propertize
                                    (format "%s[%s]"
                                            projectile-mode-line-prefix
                                            (or (projectile-project-name) "-"))
                                    'face 'eriks/mode-line-projectile-face)))
  (projectile-auto-cleanup-known-projects t)
  (projectile-find-dir-includes-top-level t)
  (projectile-current-project-on-switch 'keep)
  (projectile-ignored-project-function (cl-defun eriks/projectile-ignore-project (truename)
                                         "Ignore the sources of rust packages."
                                         (string-prefix-p (file-truename "~/.cargo") truename)))
  :config
  (defface eriks/mode-line-projectile-face nil
    "face for the projectile project in the modeline")
  (put 'projectile--mode-line 'risky-local-variable t) ;; NOTE: make mode line colors work
  (put 'projectile-project-root 'safe-local-variable #'stringp)
  (projectile-mode 1)
  :general-config
  ('projectile-mode-map
   :prefix "C-c"
   "p" 'projectile-command-map))

;; TODO: use-package proced

(use-package xref
  :general-config
  ('normal
   :prefix "g"
   "y" 'xref-find-references
   "s" 'xref-find-apropos))

(use-package find-file
  :general-config
  ('normal
   :prefix "g"
   "f" 'ff-find-related-file
   "F" 'find-file-at-point))
