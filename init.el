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
(add-to-list 'load-path (concat user-emacs-directory "requires"))

;; no more customize!
(setq-default custom-file "/tmp/emacs-custom-file")

;; load everything
(mapc #'load
      (directory-files (concat user-emacs-directory "init.d") t "\\.elc?$" nil))
