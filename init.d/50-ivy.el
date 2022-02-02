(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (eriks/unset-key ivy-switch-buffer-map "C-k")
  :general
  ('ivy-minibuffer-map
   "<escape>" 'keyboard-escape-quit
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line
   "C-u" 'ivy-kill-line
   "C-w" 'ivy-backward-kill-word
   "C-b" 'ivy-scroll-down-command
   "C-f" 'ivy-scroll-up-command
   "TAB" 'ivy-alt-done
   "M-TAB" 'ivy-partial)
  ('ivy-switch-buffer-map
   "<delete>" 'ivy-switch-buffer-kill)
  :custom
  (ivy-use-selectable-prompt t)
  (ivy-wrap t))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :custom
  (counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never --smart-case %s")
  :config
  (defun eriks/counsel-top-most-git-root-advice (fun &rest args)
    "Advice around functions that looks for the root of a git
repo using `counsel--git-root'. This advice will modify it to
look for the top-most git repo, i.e., it will not stop at a
submodule but find the whole project."
    (cl-labels ((top-git-root (&optional dir)
                              (when-let (root (counsel--dominating-file ".git" dir))
                                (let ((parent (file-name-directory (directory-file-name root))))
                                  (or (and parent (top-git-root parent))
                                      root)))))
      (cl-letf* (((symbol-function #'counsel--git-root) #'top-git-root))
        (apply fun args))))
  ;; Affects `counsel-rg' aswell since it calls `counsel-ag'
  (advice-add 'counsel-ag :around #'eriks/counsel-top-most-git-root-advice)
  (counsel-mode 1)
  :general
  ('counsel-mode-map
   [remap describe-bindings] nil
   [remap recentf-open-files] 'counsel-recentf
   "M-s" 'counsel-rg)
  ('normal
   'counsel-mode-map
   :prefix eriks/leader
   "y" 'counsel-yank-pop))

(use-package swiper
  :ensure t
  :after ivy
  :general
  ('(normal ivy-mode-map)
   "C-s" 'swiper
   "C-S-s" 'swiper-all)
  ('(normal visual)
   :prefix eriks/leader
   "s" 'swiper-thing-at-point))

(use-package ivy-xref
  :ensure t
  :after ivy
  :config
  (eriks/frames-only-use-window-funcs 'ivy-xref-show-xrefs) ;; spawns a buffer that is immediately closed
  (if (< emacs-major-version 27)
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (setq xref-show-definitions-function #'ivy-xref-show-defs)))
