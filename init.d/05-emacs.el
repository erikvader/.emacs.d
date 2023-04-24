;; looks
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default inhibit-startup-screen t)
(setq-default frame-title-format "Emacs - %b")
(setq-default cursor-type 'bar)
(setq-default blink-matching-paren nil)
(global-hl-line-mode t)
(setq-default column-number-indicator-zero-based nil)
(show-paren-mode -1)

;; scroll bars
(set-scroll-bar-mode 'right)
(add-to-list 'default-frame-alist '(scroll-bar-width . 10))
(defun eriks/disable-scroll-bar-in-current-minibuffer (&optional frame)
  (set-window-scroll-bars (minibuffer-window frame) 0 nil nil nil t))
(add-hook 'after-make-frame-functions 'eriks/disable-scroll-bar-in-current-minibuffer)
(eriks/disable-scroll-bar-in-current-minibuffer) ;; probably needed if started as non-daemon

;; variables
(setq-default line-move-visual nil)
(setq-default use-short-answers t)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default sentence-end-double-space nil)
(setq-default select-enable-clipboard nil)
(setq-default large-file-warning-threshold (* 1000 1000 20))
(setq-default tab-width 4)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default inhibit-x-resources t)

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
(add-hook 'conf-mode-hook 'maybe-enable-require-final-newline)

;; a nice fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; remove annoying keybinds
(general-unbind "<home>" "<end>" "<prior>" "<next>")

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; suppress logs from native compilation and byte compilation
(require 'warnings)
(add-to-list 'warning-suppress-types '(bytecomp))
(add-to-list 'warning-suppress-types '(comp))
