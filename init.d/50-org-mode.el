(use-package org
  :config
  (evil-set-initial-state 'org-mode 'normal)
  :custom
  (org-blank-before-new-entry '((heading) (plain-list-item)))
  (org-edit-src-content-indentation 0)
  (org-indent-indentation-per-level 1)
  (org-pretty-entities t)
  (org-src-window-setup 'current-window)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-use-sub-superscripts '{})
  (org-tags-column 0)
  :gfhook
  (nil (cl-defun org-wrap-lines-hook-fun ()
         (toggle-truncate-lines 0)
         (toggle-word-wrap 1)))
  :general
  ('org-mode-map
   "C-c s" 'counsel-org-goto-all))

(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-ellipsis "↴")
  (org-bullets-bullet-list '("▶" "▼" "◀" "▲"))
  :ghook 'org-mode-hook)

(use-package eriks-org-utils
  :after org)

(use-package evil-org
  :diminish
  :after org
  :ghook 'org-mode-hook
  :config
  (evil-org-set-key-theme '(textobjects insert additional todo))
  (defhydra eriks/hydra-evil-org-navigation (:timeout 3)
    "navigating"
    ("h" org-up-element)
    ("l" org-down-element)
    ("k" org-backward-element)
    ("j" org-forward-element)
    ("H" evil-org-top)
    ("<tab>" org-cycle))
  :general
  ('motion
   'evil-org-mode-map
   :prefix "g"
   "h" 'eriks/hydra-evil-org-navigation/org-up-element
   "l" 'eriks/hydra-evil-org-navigation/org-down-element
   "k" 'eriks/hydra-evil-org-navigation/org-backward-element
   "j" 'eriks/hydra-evil-org-navigation/org-forward-element
   "H" 'eriks/hydra-evil-org-navigation/evil-org-top)
  ('normal
   'evil-org-mode-map
   "go" (evil-org-define-eol-command org-insert-heading-respect-content)
   "gO" (evil-org-define-eol-command org-insert-subheading)
   "T"  (evil-org-define-eol-command org-insert-todo-heading-respect-content)
   "gt" (evil-org-define-eol-command org-insert-todo-subheading)))

(use-package eriks-org-spaced-mode
  :after org)
