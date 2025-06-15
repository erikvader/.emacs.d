;; looks
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default inhibit-startup-screen t)
(setq-default frame-title-format "Emacs - %b")
(setq-default cursor-type 'bar)
(setq-default blink-matching-paren nil)
(global-hl-line-mode 1)
(setq-default column-number-indicator-zero-based nil)
(show-paren-mode -1)

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
(setq-default fill-column 90)
(setq-default scroll-margin 10)
(setq-default scroll-conservatively scroll-margin)
(setq-default scroll-preserve-screen-position 'tt)
(setq-default next-screen-context-lines 10)

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

;; disable electric indent easily in file local variables
(defvar-local eriks/disable-electric-indent-local nil
  "Disables `electric-indent-mode' in a buffer if non-nil")
(put 'eriks/disable-electric-indent-local 'safe-local-variable #'booleanp)

(defun eriks/disable-electric-indent-hook-fun ()
  "Disables `electric-indent-local-mode' if `eriks/disable-electric-indent-local' is
non-nil."
  (when eriks/disable-electric-indent-local
    (electric-indent-local-mode -1)))

(add-hook 'after-change-major-mode-hook 'eriks/disable-electric-indent-hook-fun)

;; a nice fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; suppress logs from native compilation and byte compilation
(require 'warnings)
(add-to-list 'warning-suppress-types '(bytecomp))
(add-to-list 'warning-suppress-types '(comp))

;; assume that files with an actual recognized character encoding are text files
(defun eriks/use-text-mode-p ()
  (not (eq 'no-conversion buffer-file-coding-system)))
(add-to-list 'magic-fallback-mode-alist '(eriks/use-text-mode-p . text-mode) t)

;; add a more handy way to enable minor modes in file local variables
(defun eriks/symbol-list-p (x)
  (or (symbolp x)
      (and
       (listp x)
       (cl-every #'symbolp x))))

(defcustom eriks/activate-minor-modes nil
  "List of minor modes to enable from file local variables. This is
intended to be more ergonomic than `eval'.

This is permanently local since a mode will never be deactivated
the way this is implemented."
  :safe #'eriks/symbol-list-p
  :local 'permanent)

(defun eriks/apply-minor-modes-file-local ()
  (let ((modes (cond ((listp eriks/activate-minor-modes)
                      eriks/activate-minor-modes)
                     ((symbolp eriks/activate-minor-modes)
                      (list eriks/activate-minor-modes))
                     (t
                      (error "Invalid value: %s" eriks/activate-minor-modes)))))
    (dolist (mode modes)
      (if (and (functionp mode)
               (string-suffix-p "-mode" (symbol-name mode)))
          (funcall mode 1)
        (message "Not a valid minor mode: `%s', skipping..." mode)))))

(add-hook 'hack-local-variables-hook #'eriks/apply-minor-modes-file-local)
