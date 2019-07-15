;; looks
(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq-default inhibit-startup-screen t)
(setq-default frame-title-format "Emacs - %b")
(setq-default cursor-type 'bar)
(setq-default blink-matching-paren nil)
(hl-line-mode 1)

;; variables
(setq-default line-move-visual nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default sentence-end-double-space nil)

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

;; handy functions
(defun noop (&rest rest) "Does nothing, successfully!" (interactive))

;; a nice fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")
