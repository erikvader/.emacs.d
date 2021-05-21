;; looks
(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq-default inhibit-startup-screen t)
(setq-default frame-title-format "Emacs - %b")
(setq-default cursor-type 'bar)
(setq-default blink-matching-paren nil)
(global-hl-line-mode t)
(setq-default column-number-indicator-zero-based nil) ;TODO: verkar inte funka i SML

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

;; require final newlines in all prog-modes
(defun disable-require-final-newline (&optional enable)
  "Set `require-final-newline' in this buffer to nil, unless enable is non-nil (or called
with a prefix argument), then it is set to t."
  (interactive "P")
  (setq-local require-final-newline (and enable t))
  (when (called-interactively-p 'interactive)
    (message "%s %s" "require-final-newline is now" require-final-newline)))

(defun maybe-enable-require-final-newline ()
  "Set `require-final-newline' in this buffer to t unless someone else already has set it
to anything else."
  (interactive)
  (unless (local-variable-p 'require-final-newline)
    (disable-require-final-newline t)))

(add-hook 'prog-mode-hook 'maybe-enable-require-final-newline)

;; a nice fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)
