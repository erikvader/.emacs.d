;; init package
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/"))
;; NOTE: workaround for elpa
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux?utm_source=share&utm_medium=web2x
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; fix load-paths
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(mapc
 (lambda (x) (add-to-list 'load-path x))
 (directory-files (concat user-emacs-directory "submodules") t "^[^.]" t))
(add-to-list 'load-path (concat user-emacs-directory "requires"))

;; move auto saves
(defconst autosave-dir (concat user-emacs-directory "auto_saves" "/"))
(make-directory autosave-dir t)
(setq-default auto-save-file-name-transforms
      `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\1") t)))

;; move backups
(defconst backup-dir (concat user-emacs-directory "backups" "/"))
(make-directory backup-dir t)
(add-to-list 'backup-directory-alist `("." . ,backup-dir))

;; remove lockfiles
(setq-default create-lockfiles nil)

;; no more customize!
(setq-default custom-file "/tmp/emacs-custom-file")

;; load everything
(mapc #'load
      (directory-files (concat user-emacs-directory "init.d") t "\\.elc?$" nil))
