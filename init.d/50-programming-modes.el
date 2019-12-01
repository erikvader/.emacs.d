;; general
(setq-default standard-indent 3)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :ghook 'prog-mode-hook)

(defun eriks/prog-mode-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'eriks/prog-mode-show-trailing-whitespace)

(use-package cc-mode
  :defer t
  :after-config
  ('smartparens
   (eriks/sp-open-on "{" '(c-mode java-mode c++-mode)))
  ('flycheck
   (eriks/flycheck-add c-mode-hook (flycheck-mode 1))
   (eriks/flycheck-add c++-mode-hook (flycheck-mode 1)))
  :custom
  (c-basic-offset 3)
  (c-offsets-alist '((inlambda . 0) ;; indent lambda body to the left
                     (cpp-macro . 0))) ;; indent macro normally
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux")))
  :gfhook
  ('c-mode-common-hook (cl-defun c-mode-common-hook-fun ()
                         (setq-local comment-start "//")
                         (setq-local comment-end "")
                         (abbrev-mode -1))))

(use-package flycheck
  :ensure t
  :after-config-hook t
  :config
  (defmacro eriks/flycheck-add (hook &rest body)
    (let ((fun-name (intern (concat "flycheck-" (symbol-name hook) "-fun"))))
      `(add-hook (quote ,hook) (cl-defun ,fun-name () ,@body)))))

(use-package sgml-mode
  :defer t
  :config
  (put 'sgml-basic-offset 'safe-local-variable 'integerp)
  (defun close-tag-stay ()
    (interactive)
    (save-excursion
      (sgml-close-tag)))
  :gfhook
  ('html-mode-hook (cl-defun html-mode-hook-fun ()
                     (define-key html-mode-map (kbd "C-c C-e") 'close-tag-stay)
                     (define-key html-mode-map (kbd "/") nil))))

(use-package python
  :defer t
  :after-config
  ('flycheck
   (customize-set-variable 'flycheck-pylintrc (concat user-emacs-directory ".flycheck-pylintrc"))
   (eriks/flycheck-add python-mode-hook
                       (flycheck-mode 1)
                       (setq-local flycheck-check-syntax-automatically '(save mode-enable))))
  :custom
  (python-indent-offset 3)
  :general
  ('inferior-python-mode-map
   "C-d" nil)
  :gfhook
  (nil (cl-defun python-evil-shift-hook-fun ()
         (when (featurep 'evil)
           (setq-local evil-shift-width python-indent-offset)))))

(use-package pyvenv
  :ensure t
  :config
  ;; needed for this to work in eshell
  ;; (setq eshell-modify-global-environment t)
  (pyvenv-mode 1))

(use-package highlight-indent-guides
  :defer t
  :ensure t
  :diminish
  :custom
  (highlight-indent-guides-auto-character-face-perc 40)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-auto-top-character-face-perc 70)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :ghook 'python-mode-hook)

(use-package sh-mode
  :defer t
  :after-config
  ('flycheck
   (eriks/flycheck-add sh-mode-hook (flycheck-mode 1)))
  :config
  (remove-hook 'sh-mode-hook 'sh-electric-here-document-mode))

(use-package haskell-mode
  :defer t
  :diminish interactive-haskell-mode
  :ensure t
  :after-config
  ('flycheck
   (eriks/flycheck-add haskell-mode-hook
                       (flycheck-mode 1)
                       (flycheck-select-checker 'haskell-hlint)))
  :gfhook
  #'haskell-doc-mode
  #'interactive-haskell-mode)

(use-package haskell-interactive-mode
  :defer t
  :config
  (evil-set-initial-state 'haskell-interactive-mode 'normal)
  :general
  ('normal
   'haskell-interactive-mode-map
   "C-k" 'haskell-interactive-mode-history-previous
   "C-j" 'haskell-interactive-mode-history-next
   "^" 'haskell-interactive-mode-bol
   "G" "M-> ^"
   "RET" 'haskell-interactive-mode-return))

(use-package diff-mode
  :defer t
  :gfhook (nil (cl-defun diff-mode-hook-fun ()
                 (diff-auto-refine-mode -1))))

(use-package man
  :defer t
  :custom
  (Man-width 80)
  :general
  ('motion
   'Man-mode-map
   "q" 'quit-window-kill
   "f" 'man-follow
   "C-n" 'Man-next-section
   "C-p" 'Man-previous-section
   "s" 'Man-goto-section
   "<backspace>" 'evil-ex-nohighlight
   "U" 'eriks/Man-update-manpage)
  :config
  (evil-set-initial-state 'Man-mode 'motion)
  (defun eriks/Man-update-manpage ()
    "Run `Man-update-manpage' but make sure it fills to window width
by temporarily setting `Man-width' to nil."
    (interactive)
    (let ((Man-width nil))
      (Man-update-manpage)))
  :gfhook
  ('Man-mode-hook (cl-defun man-mode-hook-fun ()
                    (face-remap-set-base 'default '(:foreground "#f8f8f2")))))

(use-package rust-mode
  :ensure t
  :defer t
  :after-config
  ('smartparens
   (eriks/sp-open-on "{" 'rust-mode))
  :custom
  (rust-indent-offset 3))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after (:and rust-mode flycheck)
  :after-config
  ('flycheck
   (eriks/flycheck-add rust-mode-hook
                       (flycheck-mode 1)
                       (flycheck-rust-setup))))

(use-package ess
  :ensure t
  :defer t
  :config
  (evil-set-initial-state 'ess-r-help-mode 'motion)
  :after-config
  ('smartparens
   (eriks/sp-open-on "{" 'ess-r-mode))
  :general
  ('inferior-ess-r-mode-map
   "C-d" nil
   "C-y" nil
   [remap comint-send-input] 'inferior-ess-send-input)
  :custom
  (ess-ask-for-ess-directory nil)
  (ess-help-own-frame t)
  (ess-use-ido nil)
  (ess-use-flymake nil)
  (ess-history-file nil)
  (ess-style 'RStudio))

(use-package comint
  :defer t
  :config
  (evil-set-initial-state 'comint-mode 'normal)
  :general
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

(use-package m4-mode
  :after-config
  ('smartparens
   (sp-local-pair 'm4-mode "`" "'" :actions '(insert autoskip navigate))))

(use-package js
  :after-config
  ('smartparens
   (eriks/sp-open-on '("[" "{") 'js-mode)))
