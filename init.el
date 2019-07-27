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

;; init-warn
(defvar eriks/init-warnings-hook nil
  "Variable for `eriks/init-warn' to run stuff after initialization")
(defun eriks/init-warn (format &rest objects)
  "Behaves the same as `warn' except when called from an initializing
emacs, in which case it will postpone the call to `warn' until
initialization is done. When done a new frame will be created and
all warnings will be displayed there. This works even if emacs is
started in daemon mode."
  (if (eq (framep (selected-frame)) 'x)
      (apply #'warn format objects)
    (push `(,format ,@objects)
           eriks/init-warnings-hook)))

;; load everything
(mapc #'load
      (directory-files (concat user-emacs-directory "init.d") t "\\.elc?$" nil))

;; finalize daemon startup early so that graphical displays can be created
;; https://emacs.stackexchange.com/questions/32692/daemon-mode-defer-interactive-prompts-on-startup
;; added so that `eriks/init-warn' can work
(when (daemonp)
  (setq after-init-time (current-time)) ;; `daemon-initialized' doesn't run unless this is set
  (daemon-initialized)
  (advice-add 'daemon-initialized :override #'ignore) ;; make sure that this function isn't run again (the one called normally maybe??)
  )

;; create a frame and show all warnings from `eriks/init-warn' in there
(when eriks/init-warnings-hook
  (select-frame (make-frame '((window-system . x))))
  (switch-to-buffer "*Warnings*")
  (mapc (lambda (args) (apply #'warn args))
        eriks/init-warnings-hook))
