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
  :general
  ("C-c C-f" 'apheleia-format-buffer))

(use-package dired
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target (cl-defun eriks/dired-dwim-target ()
                       (list (dired-current-directory))))
  :config
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-collection-dired-setup)
  :gfhook 'dired-hide-details-mode)

(use-package vscode-icon
  :ensure t)

(use-package dired-subtree
  :ensure t)

(use-package dired-sidebar
  :ensure t
  :config
  (defun eriks/dired-subtree-create-empty-file ()
    "Call `dired-create-empty-file' in the current subtree."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'dired-create-empty-file)))

  (advice-add 'dired-create-empty-file :after #'dired-sidebar-refresh-buffer)

  (defun eriks/dired-sidebar-enter ()
    "Toggle subtrees on directories and visit normal files."
    (interactive)
    (if (not (dired-utils-is-dir-p))
        (dired-sidebar-find-file)
      (dired-sidebar-subtree-toggle)
      (when (dired-subtree--is-expanded-p)
        (dired-next-line 1))))

  (defun eriks/dired-sidebar-find-file ()
    "Call `find-file' in the current subtree."
    (interactive)
    (let ((default-directory (dired-current-directory))
          (cur-frame (selected-frame)))
      (call-interactively #'find-file)
      (when dired-sidebar-close-sidebar-on-file-open
        (with-selected-frame cur-frame
          (dired-sidebar-hide-sidebar)))))

  (defun eriks/dired-sidebar-subtree-cycle ()
    "Dired sidebar version of `dired-subtree-cycle', akin to
`dired-sidebar-subtree-toggle'."
    (interactive)
    (let ((last-command (if (eq last-command 'eriks/dired-sidebar-subtree-cycle)
                            'dired-subtree-cycle
                          last-command)))
      (dired-subtree-cycle)
      (dired-sidebar-redisplay-icons)))

  :custom
  (dired-sidebar-close-sidebar-on-file-open t)
  (dired-sidebar-theme 'vscode)
  :general
  ('dired-sidebar-mode-map
   "C-x C-f" #'eriks/dired-sidebar-find-file)
  ('normal
   'dired-sidebar-mode-map
   "<tab>" 'eriks/dired-sidebar-subtree-cycle
   "q" 'dired-sidebar-hide-sidebar
   "g+" 'eriks/dired-subtree-create-empty-file
   "l" 'eriks/dired-sidebar-enter
   "h" 'dired-subtree-up)
  ('normal
   :prefix eriks/leader
   "m" 'dired-sidebar-toggle-sidebar))
