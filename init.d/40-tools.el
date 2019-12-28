(use-package company
  :ensure t
  :diminish
  :custom
  (company-begin-commands nil)
  (company-idle-delay nil)
  :general
  ('(insert normal)
   "C-SPC" 'company-complete)
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

(defun eriks/prog-mode-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'eriks/prog-mode-show-trailing-whitespace)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :ghook 'prog-mode-hook)

(use-package eldoc
  :defer t
  :config
  (global-eldoc-mode)
  :diminish eldoc-mode)

;; python-mode already does this by itself
(use-package dtrt-indent
  :ensure t
  :diminish
  :config
  (dtrt-indent-global-mode))
