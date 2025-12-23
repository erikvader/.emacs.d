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
;; (setq-default tab-width 4) ;; NOTE: don't change the default
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default what-cursor-show-names t)
(setq-default inhibit-x-resources t)
(setq-default nobreak-char-display nil)
(setq-default fill-column 90)
(setq-default scroll-margin 10)
(setq-default scroll-conservatively scroll-margin)
(setq-default scroll-preserve-screen-position 'tt)
(setq-default next-screen-context-lines 10)
(setq-default auto-save-no-message t)

(defun reset-scroll-margin ()
  "Make the cursor stick to the bottom when scrolling.

I have modified `scroll-margin' and other scroll variables to play nice
when scrolling in normal text buffers visiting source files and such.
But modes, such as `comint-mode' and `compilation-mode', that append
text to the buffer and scrolls to show that new text, get unnecessary
blank lines at the bottom. It's preferable to have the bottom-most line
at the bottom window edge in those cases, so as much text as possible is
visible. This function sets the scroll variables to make this the case."
  (setq-local scroll-margin 0 ;; no margin to the bottom edge
              scroll-conservatively 200 ;; >100 disables recentering
              scroll-preserve-screen-position nil ;; probably also needed
              ))

;; A sort of global hook
(defvar eriks/editable-file-hook nil
  "A hook that is run for all buffers with human editable text.

Each mode in emacs is largely divided into three modes:
  - prog-mode for source code
  - text-mode for human text
  - special-mode for external processes and stuff

But there are for some reason exceptions, like `conf-mode', which uses
its own hiearchy of modes.

So this hook is meant to be an unified place to add functions to,
avoiding having to add them in several places.")

(defun eriks/run-editable-file-hook ()
  "Runs `eriks/editable-file-hook'"
  (run-hooks 'eriks/editable-file-hook))

(add-hook 'prog-mode-hook 'eriks/run-editable-file-hook)
(add-hook 'text-mode-hook 'eriks/run-editable-file-hook)

;; require final newlines in all prog-modes
(defun disable-require-final-newline (&optional enable)
  "Set `require-final-newline' in this buffer to nil, unless enable is non-nil (or called
with a prefix argument), then it is set to t."
  (interactive "P")
  (setq-local require-final-newline (and enable t))
  (when (called-interactively-p 'interactive)
    (message "%s %s" "require-final-newline is now" require-final-newline)))

(defun maybe-enable-require-final-newline ()
  "Set `require-final-newline' in this buffer to
`mode-require-final-newline' unless someone else already has set it to
anything else.

The major modes are supposed to enable this by themselves, but far from all do."
  (interactive)
  (unless (local-variable-p 'require-final-newline)
    (disable-require-final-newline mode-require-final-newline)))

(add-hook 'eriks/editable-file-hook 'maybe-enable-require-final-newline)

;; a nice fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; Make sure the cursor can't enter the prompt in the minibuffer
(setopt minibuffer-prompt-properties (nconc (list 'cursor-intangible t)
                                            minibuffer-prompt-properties))

;; Enable them
(setq-default enable-recursive-minibuffers t)

;; Hide commands in M-x that don't work or belong in the current mode.
(setq-default read-extended-command-predicate #'command-completion-default-include-p)

;; Ignore case when completing
(setq-default read-file-name-completion-ignore-case t
              read-buffer-completion-ignore-case t
              completion-ignore-case t)

;; assume that files with an actual recognized character encoding are text files
(defun eriks/use-text-mode-p ()
  (not (eq 'no-conversion buffer-file-coding-system)))
(add-to-list 'magic-fallback-mode-alist '(eriks/use-text-mode-p . text-mode) t)

;; add a more handy way to enable and disable minor modes in file local variables
(defcustom eriks/activate-minor-modes nil
  "List of minor modes to enable from file local variables. This is
intended to be more ergonomic than `eval'.

This is permanently local since a mode will never be deactivated
the way this is implemented."
  :safe #'eriks/symbol-list-p
  :local 'permanent)

;; TODO: this is not tested. Try with `electric-indent-local-mode'
(defcustom eriks/deactivate-minor-modes nil
  "List of minor modes to disable from file local variables. This is
intended to be more ergonomic than `eval'.

This is permanently local since a mode will never be re-activated
the way this is implemented."
  :safe #'eriks/symbol-list-p
  :local 'permanent)

(defun eriks/apply-minor-modes-file-local ()
  (let ((on-modes (ensure-list eriks/activate-minor-modes))
        (off-modes (ensure-list eriks/deactivate-minor-modes)))
    (dolist (mode on-modes)
      (if (eriks/mode-p mode)
          (funcall mode 1)
        (message "Not a valid minor mode: `%s', skipping..." mode)))
    (dolist (mode off-modes)
      (if (eriks/mode-p mode)
          (funcall mode -1)
        (message "Not a valid minor mode: `%s', skipping..." mode)))))

;; NOTE: this hook runs late in major mode initialization, see `run-mode-hooks', which is
;; run last when enabling a mode, see `define-derived-mode'.
(add-hook 'hack-local-variables-hook #'eriks/apply-minor-modes-file-local)
