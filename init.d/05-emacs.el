;; looks
(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq-default inhibit-startup-screen t)
(setq-default frame-title-format "Emacs - %b")
(setq-default cursor-type 'bar)
(setq-default blink-matching-paren nil)
(global-hl-line-mode t)

;; variables
(setq-default line-move-visual nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default sentence-end-double-space nil)
(setq-default select-enable-clipboard nil)
(setq-default large-file-warning-threshold (* 1000 1000 20))
(setq-default tab-width 4)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; require final newlines everywhere
(setq-default require-final-newline t)
(defun disable-require-final-newline ()
  (interactive)
  (setq-local require-final-newline nil)
  (when (called-interactively-p 'interactive)
    (message "%s %s" "require-final-newline is now" require-final-newline)))

;; a nice fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)

(general-def "C-c C-o" 'browse-url-at-point)
