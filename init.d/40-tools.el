(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  ;; NOTE: the non-eager one doesn't work if a function uses the callback delayed
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

;; TODO: remove various manual project.el overrides i have added, projectile integrates to
;; that itself, so no need for my advices and stuff.
;; TODO: give project.el a serious go. I don't think projectile offers anything unique i need
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
  (projectile-enable-frecency nil)
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
   ;; TODO: is `projectile-find-other-file' a better version of this?
   "f" 'ff-find-related-file
   "F" 'find-file-at-point))
