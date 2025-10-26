(use-package org
  :config
  (evil-set-initial-state 'org-mode 'normal)
  :custom
  ;; TODO: keep.org respekterar inte manuell hidden
  (org-edit-src-content-indentation 0)
  (org-pretty-entities t)
  (org-src-window-setup 'current-window)
  (org-startup-indented t)
  (org-use-sub-superscripts '{})
  (org-tags-column 0)
  :gfhook
  (nil (cl-defun org-wrap-lines-hook-fun ()
         (toggle-truncate-lines 0)
         (toggle-word-wrap 1))))

(use-package evil-org
  :ensure t
  :diminish
  :ghook 'org-mode-hook)
