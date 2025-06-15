;; init package
(require 'package)
(if (bound-and-true-p eriks/offline)
    (setq package-archives nil)
  (add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(message "Using these package archives: %s" package-archives)

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

;; load common lisp for everyone!
(require 'cl-lib)

;; load everything
(cl-flet ((list-el-files (dir) (directory-files dir t "\\.elc?$" nil)))
  (mapc #'load
        (sort (nconc
               (list-el-files (concat user-emacs-directory "init.d"))
               (let ((local-config (concat user-emacs-directory "init.d.local")))
                 (when (file-exists-p local-config)
                   (list-el-files local-config))))
              (lambda (f1 f2)
                (string< (file-name-nondirectory f1)
                         (file-name-nondirectory f2))))))
