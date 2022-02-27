(defun eriks/show-paren-alt-hook ()
  (face-remap-set-base 'show-paren-match '(:underline t)))

(use-package cc-mode
  :defer t
  :config
  (eriks/sp-open-on "{" '(c-mode java-mode c++-mode))
  (c-add-style "eriks-java"
               '("java" (c-offsets-alist
                         (arglist-intro . +)
                         (arglist-close . 0))))
  ;TODO: for C++ or something
  ;; (inlambda . 0) ;; indent lambda body to the left
  ;; (cpp-macro . 0) ;; indent macro normally))
  :custom
  (c-basic-offset 4)
  (c-default-style '((java-mode . "eriks-java")
                     (awk-mode . "awk")
                     (other . "linux")))
  :gfhook
  ('c-mode-common-hook (cl-defun c-mode-common-hook-fun ()
                         (setq-local comment-start "//")
                         (setq-local comment-end "")
                         (abbrev-mode -1)))
  :general
  ('normal
   '(c-mode-map c++-mode-map)
   "gf" #'ff-find-other-file))

(use-package cmake-mode
  :ensure t)

(use-package sgml-mode
  :config
  (put 'sgml-basic-offset 'safe-local-variable 'integerp)
  (evil-set-initial-state 'sgml-mode 'normal)
  :gfhook
  ('sgml-mode-hook #'sgml-electric-tag-pair-mode))

(use-package mhtml-mode
  :config
  (add-to-list 'sp-navigate-consider-sgml-tags 'mhtml-mode)
  :gfhook
  'eriks/show-paren-alt-hook)

(use-package nxml-mode
  :config
  (evil-set-initial-state 'nxml-mode 'normal)
  :gfhook
  ('nxml-mode-hook #'sgml-electric-tag-pair-mode))

(use-package python
  :defer t
  :general
  ('inferior-python-mode-map
   "C-d" nil))

(use-package pyvenv
  :ensure t
  :config
  ;; needed for this to work in eshell
  ;; (setq eshell-modify-global-environment t)
  (pyvenv-mode 1))

(use-package sh-script
  :defer t
  :config
  (eriks/sp-open-on "{" 'sh-mode)
  (remove-hook 'sh-mode-hook 'sh-electric-here-document-mode))

(use-package haskell-mode
  :defer t
  :diminish interactive-haskell-mode
  :ensure t
  :gfhook
  #'haskell-doc-mode
  #'interactive-haskell-mode)

(use-package hindent
  :ensure t
  :general
  ('normal
   'haskell-mode-map
   "M-q" 'hindent-reformat-decl-or-fill)
  ('visual
   'haskell-mode-map
   "M-q" 'hindent-reformat-region))

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
   "<return>" 'haskell-interactive-mode-return))

(use-package diff-mode
  :defer t
  :custom
  (diff-refine nil))

(use-package man
  :defer t
  :general
  ('motion
   'Man-mode-map
   "q" 'quit-window-kill
   "f" 'man-follow
   "C-n" 'Man-next-section
   "C-p" 'Man-previous-section
   "s" 'Man-goto-section
   "<backspace>" 'evil-ex-nohighlight
   "d" 'scroll-up-command
   "u" 'scroll-down-command
   "U" 'Man-update-manpage)
  :config
  (evil-set-initial-state 'Man-mode 'motion)
  :gfhook
  ('Man-mode-hook 'scroll-lock-mode)
  ('Man-mode-hook (cl-defun man-mode-hook-fun ()
                    (face-remap-set-base 'default '(:foreground "#f8f8f2")))))

(use-package make-mode
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (eriks/sp-open-on "{" 'rust-mode)
  :general
  ('rust-mode-map
   "C-c C-f" nil))

(use-package ess
  :ensure t
  :defer t
  :config
  (evil-set-initial-state 'ess-r-help-mode 'motion)
  (eriks/sp-open-on "{" 'ess-r-mode)
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

(use-package m4-mode
  :config
  (sp-local-pair 'm4-mode "`" "'" :actions '(insert autoskip navigate)))

(use-package js
  :config
  (eriks/sp-open-on '("[" "{") 'js-mode))

(use-package json-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'sp-navigate-consider-sgml-tags 'rjsx-mode)
  :gfhook
  'eriks/show-paren-alt-hook)

(use-package typescript-mode
  :ensure t
  :gfhook
  ('typescript-mode-hook (cl-defun eriks/typescript-hook-fun ()
                           (setq-local comment-start-skip "\\(//+\\|/?\\*+\\)\\s *")
                           (setq-local eriks/evil-open-line-comment-fun
                                       (lambda ()
                                         (js2-line-break)
                                         (indent-according-to-mode)))))
  :config
  (eriks/sp-open-on '("[" "{") 'typescript-mode))

(use-package css-mode
  :config
  (eriks/sp-open-on "{" 'css-mode))

(use-package elixir-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 4)
  :config
  (sp-local-pair 'lua-mode "if" nil :actions nil)
  (sp-local-pair 'lua-mode "while" nil :actions nil)
  (sp-local-pair 'lua-mode "for" nil :actions nil)
  (sp-local-pair 'lua-mode "function" nil :actions nil))

(use-package minizinc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode)))

(use-package slime
  :ensure t
  :custom
  (inferior-lisp-program "sbcl")
  (slime-contribs '(slime-fancy slime-asdf slime-quicklisp))
  :config
  (eriks/add-to-lists 'interpreter-mode-alist
                      '("sbcl" . lisp-mode)
                      '("sbcl-script" . lisp-mode))
  (eriks/frames-only-use-window-funcs 'sldb-setup)
  (evil-set-initial-state 'slime-repl-mode 'normal)
  (defun eriks/slime-load-current-file ()
    "Runs `slime-load-file' with the current buffer file."
    (interactive)
    (unless (buffer-file-name)
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (slime-load-file (buffer-file-name)))
  (function-put 'iter 'common-lisp-indent-function '(nil)) ;; don't indent iter like a defun
  :general
  ('slime-mode-indirect-map
   "C-c C-l" 'eriks/slime-load-current-file)
  ('slime-repl-mode-map
   "C-l" 'slime-repl-clear-buffer)
  ('normal
   'slime-repl-mode-map
   "C-k" 'slime-repl-previous-input
   "C-j" 'slime-repl-next-input
   "<return>" 'slime-repl-return)
  ('insert
   'slime-repl-mode-map
   "<up>" 'slime-repl-previous-input
   "<down>" 'slime-repl-next-input))
