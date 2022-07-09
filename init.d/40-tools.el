(use-package company
  :ensure t
  :diminish
  :custom
  (company-begin-commands nil)
  (company-idle-delay nil)
  :general
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
  :defer t
  :config
  (evil-set-initial-state 'comint-mode 'normal)
  :general
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
  :defer t
  :ghook 'prog-mode-hook)

(use-package eldoc
  :defer t
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
  :general
  ("C-c C-f" 'apheleia-format-buffer))
