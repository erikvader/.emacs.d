(defun eriks/show-paren-alt-hook ()
  (face-remap-set-base 'show-paren-match '(:underline t)))

(use-package cc-mode
  :config
  (eriks/sp-open-on "{" '(c-mode java-mode c++-mode))
  (c-add-style "eriks-java"
               '("java" (c-offsets-alist
                         (arglist-intro . +)
                         (arglist-close . 0))))
  ;;TODO: for C++ or something
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
  :general-config
  ('normal
   '(c-mode-map c++-mode-map)
   "gf" #'ff-find-other-file))

(use-package cmake-mode
  :ensure t)

(use-package sgml-mode
  :config
  (put 'sgml-basic-offset 'safe-local-variable 'integerp)
  (evil-set-initial-state 'sgml-mode 'normal)
  (sp-local-pair '(mhtml-mode sgml-mode html-mode) "<" ">" :actions '(navigate autoskip))

  (defun eriks/sgml-close-tag-inline ()
    "Closes a tag like `nxml-balanced-close-start-tag-inline'"
    (interactive)
    (insert ">")
    (save-excursion
      (sgml-close-tag)))

  (defun eriks/sgml-close-tag-block ()
    "Closes a tag like `nxml-balanced-close-start-tag-block'"
    (interactive)
    (eriks/sgml-close-tag-inline)
    (newline)
    (eriks/create--newline-and-enter-sexp))
  :general-config
  ('insert
   'sgml-mode-map
   "C-v" 'eriks/sgml-close-tag-inline
   "C-b" 'eriks/sgml-close-tag-block)
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
  :general-config
  ('insert
   'nxml-mode-map
   "C-v" #'nxml-balanced-close-start-tag-inline
   "C-b" #'nxml-balanced-close-start-tag-block)
  :gfhook
  ('nxml-mode-hook #'sgml-electric-tag-pair-mode))

(use-package python
  :gfhook #'subword-mode
  :general-config
  ('inferior-python-mode-map
   "C-d" nil))

(use-package pyvenv
  :ensure t
  :config
  ;; needed for this to work in eshell
  ;; (setq eshell-modify-global-environment t)
  (pyvenv-mode 1))

(use-package sh-script
  :config
  (eriks/sp-open-on "{" 'sh-mode)
  (remove-hook 'sh-mode-hook 'sh-electric-here-document-mode))

(use-package haskell-mode
  :ensure t
  :config
  (remove-hook 'haskell-mode-hook #'interactive-haskell-mode))

;;TODO: this is broken?
;;(use-package hindent
;;  :ensure t
;;  :general-config
;;  ('normal
;;   'haskell-mode-map
;;   "M-q" 'hindent-reformat-decl-or-fill)
;;  ('visual
;;   'haskell-mode-map
;;   "M-q" 'hindent-reformat-region))

(use-package diff-mode
  :custom
  (diff-refine nil))

(use-package man
  :general-config
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

;;TODO: finns det en c-end-of-statement osv för rust?
(use-package rust-mode
  :ensure t
  :config
  (eriks/sp-open-on "{" 'rust-mode)
  :general-config
  ('rust-mode-map
   "C-c C-f" nil))

(use-package ess
  :ensure t
  :config
  (evil-set-initial-state 'ess-r-help-mode 'motion)
  (eriks/sp-open-on "{" 'ess-r-mode)
  :general-config
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
  (slime-contribs '(slime-fancy slime-asdf slime-quicklisp))
  :config
  (setq-default inferior-lisp-program "sbcl")
  (add-to-list 'auto-mode-alist '("/\\.sbclrc\\'" . lisp-mode))
  (eriks/frames-only-use-window-funcs 'sldb-setup)
  (evil-set-initial-state 'slime-repl-mode 'normal)
  (function-put 'iter 'common-lisp-indent-function '(nil)) ;; don't indent iter like a defun

  (defun eriks/slime-load-current-file ()
    "Runs `slime-load-file' with the current buffer file."
    (interactive)
    (unless (buffer-file-name)
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (slime-load-file (buffer-file-name)))

  (defun eriks/slime-ctrl-d ()
    "Acts like Ctrl+d in a terminal when something is reading stdin."
    (interactive)
    (slime-repl-send-input))

  (defun eriks/slime-cd-here ()
    (interactive)
    (slime-cd default-directory))

  :general-config
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
   "<down>" 'slime-repl-next-input
   "C-d" 'eriks/slime-ctrl-d))

(use-package conf-mode
  :config
  (eriks/sp-open-on '("[" "{") 'conf-mode)
  (mapcar (lambda (ext)
            (add-to-list 'auto-mode-alist (cons ext 'conf-unix-mode)))
          '("\\.service\\'"
            "\\.timer\\'"
            "\\.target\\'"
            "\\.mount\\'"
            "\\.automount\\'"
            "\\.slice\\'"
            "\\.socket\\'"
            "\\.path\\'"
            "\\.netdev\\'"
            "\\.network\\'"
            "\\.link\\'"))
  :gfhook
  ('conf-mode-hook 'eriks/run-prog-mode-hooks))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :custom
  (cider-show-error-buffer nil)
  :general-config
  ('normal
   'cider-repl-mode-map
   "C-k" #'cider-repl-previous-input
   "C-j" #'cider-repl-next-input))

(use-package clj-refactor
  :ensure t
  :gfhook
  ('clojure-mode-hook 'clj-refactor-mode))

(use-package just-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package lisp-mode
  :gfhook ('lisp-data-mode-hook (cl-defun eriks/lisp-comment-start ()
                                  (setq-local comment-start ";;"))))

(use-package elisp-mode
  :gfhook ('emacs-lisp-mode-hook 'apheleia-mode))
