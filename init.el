;; init package
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/"))
(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; fix load-paths
;; TODO: göra en const som pekar på directoriet ist?
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(mapc
 (lambda (x) (add-to-list 'load-path x))
 (directory-files (concat user-emacs-directory "submodules") t "^[^.]" t))

;; no more customize!
(setq-default custom-file "/tmp/emacs-custom-file")

;; load everything
(mapc #'load
      (directory-files (concat user-emacs-directory "init.d") t "\\.elc?$" nil))
