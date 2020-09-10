;; init package
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/"))
(when (< emacs-major-version 27)
  (package-initialize))

;; facilities for checking how long ago it was since a
;; `package-refresh-contents' was run
(defvar eriks/package-last-refresh-file (concat user-emacs-directory ".package-last-refresh")
  "File where the timestamp of when the last time `package-refresh-contents' was called.")

(defun eriks/save-package-refresh (&rest _args)
  "Saves that package contents were refreshed now."
  (with-temp-file eriks/package-last-refresh-file
    (insert (format-time-string "%s"))))

(advice-add 'package-refresh-contents :before 'eriks/save-package-refresh)

(defun eriks/refresh-package-p (days)
  "Returns non-nil if it was atleast DAYS days since the last time
package contents were last refreshed (`package-refresh-contents').

If `eriks/package-last-refresh-file' doesn't exists, then is
non-nil returned because it is assumed that a package refresh has
never happened (DAYS is actually compared against 1970-01-01, so
it should always be non-nil unless DAYS is really large)."
  (let ((last-refresh (if (file-exists-p eriks/package-last-refresh-file)
                          (with-temp-buffer
                            (insert-file-contents eriks/package-last-refresh-file)
                            (string-to-number (buffer-string)))
                        0))
        (now (string-to-number (format-time-string "%s"))))
    (> now (+ last-refresh (* days 60 60 24)))))

;; initialize `package-user-dir' if it doesn't exist
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
