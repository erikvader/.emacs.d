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
  :defer t
  :ensure t
  :custom
  (flycheck-pylintrc (concat user-emacs-directory ".flycheck-pylintrc"))
  :ghook
  'c-mode-hook
  'c++-mode-hook
  'sh-mode-hook
  :gfhook
  ('python-mode-hook (cl-defun python-flycheck-hook-fun ()
                       (flycheck-mode 1)
                       (setq-local flycheck-check-syntax-automatically '(save mode-enable))))
  ('haskell-mode-hook (cl-defun haskell-flycheck-hook-fun ()
                        (flycheck-mode 1)
                        (flycheck-select-checker 'haskell-hlint))))

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
  :custom
  (python-indent-offset 3)
  :gfhook
  (nil (cl-defun python-evil-shift-hook-fun ()
         (when (featurep 'evil)
           (setq-local evil-shift-width python-indent-offset)))))

(use-package pyvenv
  :ensure t
  :after python
  :config
  ;; needed for this to work in eshell
  (setq eshell-modify-global-environment t)
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
  :config
  (remove-hook 'sh-mode-hook 'sh-electric-here-document-mode))

(use-package haskell-mode
  :defer t
  :ensure t
  :gfhook #'haskell-doc-mode)

(use-package diff-mode
  :defer t
  :gfhook (nil (cl-defun diff-mode-hook-fun ()
                 (diff-auto-refine-mode -1))))

(use-package man
  :defer t
  :init
  (eriks/evil-set-initial-state 'Man-mode 'motion)
  :general
  ('motion
   'Man-mode-map
   "f" 'man-follow
   "C-n" 'Man-next-section
   "C-p" 'Man-previous-section
   "s" 'Man-goto-section
   "<backspace>" 'evil-ex-nohighlight)
  :gfhook
  ('Man-mode-hook (cl-defun man-mode-hook-fun ()
                    (face-remap-set-base 'default '(:foreground "#f8f8f2")))))

(use-package rust-mode
  :ensure t
  :defer t
  :custom
  (rust-indent-offset 3))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after (:and rust-mode flycheck)
  :gfhook
  ('rust-mode-hook (cl-defun rust-flycheck-hook-fun ()
                     (flycheck-mode 1)
                     (flycheck-rust-setup))))

(use-package ess
  :ensure t
  :defer t
  :init
  (eriks/evil-set-initial-state 'ess-r-help-mode 'motion)
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
  :init
  (eriks/evil-set-initial-state 'comint-mode 'normal)
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
