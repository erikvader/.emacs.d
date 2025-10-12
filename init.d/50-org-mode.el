(use-package org
  :config
  (evil-set-initial-state 'org-mode 'normal)
  :custom
  (org-edit-src-content-indentation 0)
  (org-pretty-entities t)
  (org-src-window-setup 'current-window)
  (org-startup-indented t)
  (org-use-sub-superscripts '{})
  (org-tags-column 0)
  :gfhook
  (nil (cl-defun org-wrap-lines-hook-fun ()
         (toggle-truncate-lines 0)
         (toggle-word-wrap 1)))
  :general-config
  ('org-mode-map
   [remap org-goto] 'counsel-org-goto))

(use-package evil-org
  :ensure t
  :diminish
  :ghook 'org-mode-hook)
