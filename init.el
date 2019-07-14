;; init package
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/"))
(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; fix load-paths
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(mapc
 (lambda (x) (add-to-list 'load-path x))
 (directory-files (concat user-emacs-directory "submodules") t "^[^.]" t))

;; load everything
(mapc #'load (directory-files (concat user-emacs-directory "init.d") t "\\.elc?$" nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key counsel ivy magit drag-stuff smart-mode-line use-package golden-ratio-scroll-screen general evil diminish)))
 '(use-package-always-demand t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
