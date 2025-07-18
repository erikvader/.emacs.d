(use-package company
  :ensure t
  :diminish
  :custom
  (company-begin-commands nil)
  (company-idle-delay nil)
  :general-config
  ('(insert normal)
   'company-mode-map
   "C-SPC" 'company-complete
   "C-S-SPC" 'company-other-backend)
  ('company-active-map
   "<escape>" 'company-abort
   "C-j" 'company-select-next
   "C-k" 'company-select-previous)
  :config
  (global-company-mode)
  ;; fix https://github.com/company-mode/company-mode/issues/15
  ;; (eval-after-load 'company
  ;;   '(evil-declare-change-repeat 'company-complete))
  )

(use-package comint
  :config
  (evil-set-initial-state 'comint-mode 'normal)
  :general-config
  ('comint-mode-map
   "C-c C-k" 'comint-clear-buffer)
  ('normal
   'comint-mode-map
   "C-k" 'comint-previous-input
   "C-j" 'comint-next-input
   "<return>" 'comint-send-input)
  ('motion
   'comint-mode-map
   "G" 'end-of-buffer
   "^" 'comint-bol)
  ('insert
   'comint-mode-map
   "<up>" 'comint-previous-input
   "<down>" 'comint-next-input))

(use-package rainbow-delimiters
  :ensure t
  :ghook 'prog-mode-hook)

(use-package eldoc
  :config
  (global-eldoc-mode)
  :diminish eldoc-mode)

;TODO: how does this interact with tramp?
(use-package add-node-modules-path
  :ensure t
  :config
  (add-to-list 'safe-local-eval-forms '(add-node-modules-path)))

(use-package apheleia
  :ensure t
  :diminish "Aph"
  :general-config
  ("C-c C-f" 'apheleia-format-buffer))

(use-package projectile
  :ensure t
  :diminish
  :custom
  (projectile-current-project-on-switch 'keep)
  (projectile-ignored-project-function (cl-defun eriks/projectile-ignore-project (truename)
                                         "Ignore the sources of rust packages."
                                         (string-prefix-p (file-truename "~/.cargo") truename)))
  :config
  (put 'projectile-project-root 'safe-local-variable #'stringp)
  (projectile-mode 1)
  :general-config
  ('projectile-mode-map
   :prefix "C-c"
   "p" 'projectile-command-map))
