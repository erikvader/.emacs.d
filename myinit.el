
;;; fonts

;; tilde fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)

(defface eriks-fix-later-face
  '((t :foreground "orange1"
       :background "gray10"
       :weight bold
       ))
  "Face to highlight FIXME and TODO")

(defun add-todo-font-lock ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\):" 1 'eriks-fix-later-face t)
                            ("\\<\\(TODO\\):" 1 'eriks-fix-later-face t))))

(defface hi-red
  '((t :foreground "black"
       :background "tomato"))
  "red highlight face")

(defface hi-cyan
  '((t :foreground "black"
       :background "cyan2"))
  "cyan highlight face")

(defun show-paren-alt-hook ()
  (face-remap-set-base 'show-paren-match '(:underline t)))

(mapc (lambda (m) (add-hook m 'show-paren-alt-hook)) '(html-erb-mode-hook jinja2-mode-hook web-mode-hook nxml-mode-hook nxhtml-mode-hook rhtml-mode-hook sgml-mode-hook html-mode-hook mhtml-mode-hook))

(require 'heaven-and-hell)
;; Default is 'light
(setq heaven-and-hell-theme-type 'dark)

;; Set preferred light and dark themes
;; default light is emacs default theme, default dark is wombat
;; Themes can be the list: (dark . (tsdh-dark tango-dark))
(setq heaven-and-hell-themes
      '((light . nil)
        (dark . dracula)))

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)

;; Set keys to toggle theme and return to default emacs theme
(global-set-key (kbd "C-<f6>") 'heaven-and-hell-load-default-theme)
(global-set-key (kbd "<f6>") 'heaven-and-hell-toggle-theme)

;;; my-keys-map

(setq my-keys-map (make-sparse-keymap)) ;;varför gör jag såhär?

(define-minor-mode my-keys-minor-mode
  "my keymode to activate my keybindings"
  :global t
  :init-value t
  :lighter " my-keys"
  :keymap my-keys-map)

(my-keys-minor-mode 1)

(global-set-key (kbd "<f1>") 'my-keys-minor-mode)

;; makes ure my-keys-map always is first
(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes. Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

;;; random
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(setq inhibit-startup-screen t)

(define-key indent-rigidly-map (kbd "h") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd "l") 'indent-rigidly-right)

(defvar autosave-dir (concat "~/.emacs_auto_saves" "/"))
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms
      `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\1") t)))

;; remove some annoying keybindings
(global-set-key (kbd "<home>") nil)
(global-set-key (kbd "<end>") nil)
(global-set-key (kbd "<prior>") nil)
(global-set-key (kbd "<next>") nil)

(define-key my-keys-map (kbd "C-+") 'er/expand-region)
(define-key my-keys-map (kbd "M-s q") 'isearch-query-replace-regexp)
(define-key my-keys-map (kbd "M-s r") 'isearch-forward-regexp)
(define-key my-keys-map (kbd "M-s R") 'isearch-backward-regexp)
(define-key my-keys-map (kbd "M-s s") 'isearch-forward)
(define-key my-keys-map (kbd "M-s S") 'isearch-backward)

(setq line-move-visual nil)
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open")

(defun noop () "Does pretty much (exactly) nothing" (interactive))

;;; my custom functions
(defun eval-last-sexp-replace ()
  "Runs `eval-last-sexp' and replaces the sexp with the evaluated value"
  (interactive)
  (let ((start (point)))
    (setq current-prefix-arg '(4))
    (call-interactively 'eval-last-sexp)
    (save-excursion
      (goto-char start)
      (let ((sexp-end (point)))
        (backward-sexp)
        (delete-region (point) sexp-end)))))

(define-key my-keys-map (kbd "C-x M-e") 'eval-last-sexp-replace)

(defun eriks-up-indentation ()
  (interactive)
  (back-to-indentation)
  (let ((start (current-column)))
    (while (and
            (> (current-column) 0)
            (>= (current-column) start))
      (forward-line -1)
      (back-to-indentation))))

(defun eriks-up-same-column (&optional dir)
  (interactive)
  (let ((start (current-column))
        (prev (point))
        (dir (if (null dir) -1 dir)))
    (while (and
            (= (move-to-column start) start))
      (setq prev (point))
      (forward-line dir))
    (goto-char prev)))

(defun eriks-down-same-column ()
  (interactive)
  (eriks-up-same-column 1))

(global-set-key (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)
(define-key universal-argument-map (kbd "C-u") nil)

(defun eriks/delete-empty-parens ()
  "Delete an empty set om parens (anything in the parens syntax class).
Point can be immediately after the closing paren, inside the parens or
immediately before the opening paren."
  (interactive)
  (save-excursion
    (let ((done nil)
          (ok nil)
          (i 0))
      (while (and
              (not done)
              (< i 3))
        (when (looking-at-p "\\s(\\s)\\|\\s\"\\s\"\\|\\s/\\s/\\|\\s$\\s$\\|\\s|\\s|") ; delimeters, strings, character delimeter, paired delimeter, generic string delimiter
          (delete-region (point) (+ 2 (point)))
          (setq done t ok t))
        (setq i (1+ i))
        (setq done (or done
                       (not (ignore-errors (progn (backward-char) t))))))
      (when (not ok)
        (message "There wasn't anything to remove...")))))

(defun is-whitespace (char &optional NOTNEWLINE)
  "Checks whether char is a space, newline or tab.
if NOTNEWLINE, then don't count newlines as whitespace."
  (or
   (= char 32) ;;space
   (and (not NOTNEWLINE) (= char 10)) ;;newline
   (= char 9)  ;; tab
   ;;(bolp)
   ;;(eolp)
   ))

(defun eriks-skip-space (ARG &optional SAMELINE)
  "Move point forward or backward until it doesnt encounter whitespace anymore.
if SAMELINE then don't move the cursor between lines."
  (interactive "p")
  (if (< ARG 0)
      (while (is-whitespace (preceding-char) SAMELINE)
        (left-char))
    (while (is-whitespace (following-char) SAMELINE)
      (right-char))))

(defun eriks-skip-space-backwards (&optional SAMELINE)
  (interactive)
  (eriks-skip-space -1 SAMELINE))

(defmacro same-buffer (&rest BODY)
  "Executes BODY while forcing `switch-to-buffer-other-window' and
`display-buffer' to always open in the current window, if possible
(see `switch-to-buffer' and its last argument FORCE-SAME-WINDOW
when set to nil)."
  `(cl-letf (((symbol-function 'switch-to-buffer-other-window)
              (lambda (BUFFER-OR-NAME &rest args)
                (switch-to-buffer BUFFER-OR-NAME)))
             ((symbol-function 'display-buffer)
              (lambda (BUFFER-OR-NAME &rest args)
                (get-buffer-window (switch-to-buffer BUFFER-OR-NAME)))))
     ,@BODY))

;;; eriks-map
(define-prefix-command 'eriks-map)
(define-key my-keys-map (kbd "S-SPC") 'eriks-map)
(define-key eriks-map (kbd "C-f") 'ff-find-other-file)

(define-key eriks-map (kbd "gs") 'magit-status)
(define-key eriks-map (kbd "gb") 'magit-blame)
(define-key eriks-map (kbd "gt") 'git-timemachine)
(define-key eriks-map (kbd "gv") 'vc-annotate)

(define-key eriks-map (kbd "U") 'counsel-unicode-char)

(define-key eriks-map (kbd "t U") 'untabify)
(define-key eriks-map (kbd "t u") 'tabify)
(define-key eriks-map (kbd "t w") 'whitespace-cleanup)
(define-key eriks-map (kbd "t e") 'delete-trailing-whitespace)
;;; packages
;;;; requires
(require 'smart-mode-line)
(require 'cc-mode)
(require 'seq)
(require 'multiple-cursors)
(require 'evil)
(require 'evil-numbers)
;;(require 'framemove)
(require 'smartparens-config)
(require 'outshine)
(require 'rotate-text)
;; (require 'eyebrowse)
;;(require 'switch-window)

;;;; random
(sml/setup)

(which-key-mode 1)

(winner-mode 1)

;; (global-linum-mode)
;; (linum-relative-toggle)

(yas-global-mode 1)

(global-undo-tree-mode 1)

;; (define-key my-keys-map [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
;; (define-key my-keys-map [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

(projectile-global-mode t)
(counsel-projectile-mode)

(add-hook 'ggtags-mode-hook
          (lambda ()
            (setq ggtags-mode-line-project-name nil)))

;; disable linum-mode in main config file (this one)
;; (add-hook 'outline-minor-mode-hook (lambda ()
;;                                      (when (and
;;                                             buffer-file-name
;;                                             (equal "myinit.el"
;;                                                     (replace-regexp-in-string "\\(^.*/\\).*?$" "" buffer-file-name nil nil 1)))
;;                                        (linum-mode -1))))
;;;; ivy, swiper and counsel
(ivy-mode 1)
(counsel-mode 1)
(define-key my-keys-map (kbd "C-s") 'swiper)
(define-key ivy-minibuffer-map (kbd "<escape>") 'keyboard-escape-quit)
;; compatability with dwm-mode
;; (define-key ivy-mode-map [remap switch-to-buffer] nil)
;; (define-key ivy-mode-map [remap switch-to-buffer-other-window] nil)
(setq ivy-use-selectable-prompt t)

;; no longer requires a match, can create files now
(defun counsel-projectile-find-file (&optional arg)
  "Jump to a file in the current project.

With a prefix ARG, invalidate the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (projectile-current-project-files)
            :matcher counsel-projectile-find-file-matcher
            :require-match nil
            :sort counsel-projectile-sort-files
            :action counsel-projectile-find-file-action
            :caller 'counsel-projectile-find-file))

;;;; avy
(setq avy-keys '(;;nconc
                ?h ?g ?j ?f ?k ?d ?l ?s
                ;; (number-sequence ?a ?z)
                ;; '(?å ?ä ?ö)
                ))

;; no timeout ("infinite" timeout)
(setq avy-timeout-seconds nil)

(defun eriks/avy-binary-search ()
  "Do a binary search to find a character on the current line"
  (interactive)
  (cl-labels ((b-search (min max)
                        (when (< min max)
                          (let* ((avy-keys '(?j ?k ?l))
                                 points
                                 (middle (floor (+ min max) 2)))
                            (goto-char middle)
                            ;; special case when only two candidates
                            (if (not (= middle min))
                                (setq points (list min middle max))
                              (setq points (list middle max))
                              (setq avy-keys '(?j ?l)))
                            (avy--process points (avy--style-fn avy-style))
                            (cond
                             ((= (point) middle))
                             ((< (point) middle)
                              (b-search min (1- middle)))
                             ((> (point) middle)
                              (b-search (1+ middle) max)))))))
    (let* ((beg (save-excursion
                  (back-to-indentation)
                  (point)))
           (end (save-excursion
                  (move-end-of-line nil)
                  (point))))
      (b-search beg end))))

(define-key eriks-map (kbd ",") 'eriks/avy-binary-search)

;;;; smartparens
;; remove defaults
(sp-pair "\\\\(" nil :actions :rem)
(sp-pair "\\{"   nil :actions :rem)
(sp-pair "\\("   nil :actions :rem)
(sp-pair "\\\""  nil :actions :rem)

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair '(c-mode java-mode css-mode js-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;;;; outshine
;; (setq outshine-use-speed-commands t)
(add-hook 'outline-minor-mode-hook (lambda ()
                                     (outshine-hook-function)
                                     (define-key outline-minor-mode-map [remap self-insert-command] nil) ;;remove annyoing remap to outshine-self-insert-command
                                     ))

;; removed top-level sexpressions as outlines in lisp modes
(dolist (l-mode '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook l-mode (lambda ()
                     (setq outline-regexp ";;;\\(;* [^ 	
]\\|###autoload\\)"))))

;; enable in some programming modes
(dolist (p-mode '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  c-mode-hook
                  java-mode-hook))
  (add-hook p-mode 'outline-minor-mode))

;;;; ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ;;("perl" (mode . cperl-mode))
               ;;("erc" (mode . erc-mode))
               ;; ("planner" (or
               ;;             (name . "^\\*Calendar\\*$")
               ;;             (name . "^diary$")
               ;;             (mode . muse-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*dashboard\\*$")
                         (mode . help-mode)
                         (name . "^\\*Customize.*")))
               ("magit" (name . "^\\*magit:.*"))
               ;; ("gnus" (or
               ;;          (mode . message-mode)
               ;;          (mode . bbdb-mode)
               ;;          (mode . mail-mode)
               ;;          (mode . gnus-group-mode)
               ;;          (mode . gnus-summary-mode)
               ;;          (mode . gnus-article-mode)
               ;;          (name . "^\\.bbdb$")
               ;;          (name . "^\\.newsrc-dribble")))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(define-key my-keys-map (kbd "C-x C-b") 'ibuffer)

;;;; latex

(require 'latex)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; (modify-syntax-entry ?$ "\"" LaTeX-mode-syntax-table) ;;make $ act like string so smartparens can navigate with it.
            (define-key LaTeX-mode-map [remap beginning-of-defun] 'LaTeX-find-matching-begin)
            (define-key LaTeX-mode-map [remap end-of-defun] 'LaTeX-find-matching-end)
            (run-hooks 'prog-mode-hook)))

;;;; company
(global-company-mode t)
(define-key my-keys-map (kbd "C-SPC") 'company-complete)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;;;; windows
;; (require 'dwm)
;; (dwm-mode 1)

(require 'ace-window)
(define-key my-keys-map (kbd "C-x o") 'ace-window)

(frames-only-mode)
(setq frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops nil)

(define-key my-keys-map (kbd "S-<right>") 'windmove-right)
(define-key my-keys-map (kbd "S-<left>") 'windmove-left)
(define-key my-keys-map (kbd "S-<up>") 'windmove-up)
(define-key my-keys-map (kbd "S-<down>") 'windmove-down)

(define-key my-keys-map (kbd "C-<right>") 'enlarge-window-horizontally)
(define-key my-keys-map (kbd "C-<left>") 'shrink-window-horizontally)
(define-key my-keys-map (kbd "C-<up>") 'enlarge-window)
(define-key my-keys-map (kbd "C-<down>") 'shrink-window)

(define-key my-keys-map (kbd "C-S-<right>") 'buf-move-right)
(define-key my-keys-map (kbd "C-S-<left>") 'buf-move-left)
(define-key my-keys-map (kbd "C-S-<up>") 'buf-move-up)
(define-key my-keys-map (kbd "C-S-<down>") 'buf-move-down)

(define-key my-keys-map (kbd "C-x 1") 'delete-other-visible-frames)
(define-key my-keys-map (kbd "C-x 2") 'make-frame-command)
(define-key my-keys-map (kbd "C-x 0") 'delete-frame)
(define-key my-keys-map (kbd "C-x C-0") 'kill-buffer-and-frame)

(define-key my-keys-map (kbd "C-x 4 1") 'delete-other-windows)
(define-key my-keys-map (kbd "C-x 4 2") 'split-window-below)
(define-key my-keys-map (kbd "C-x 4 0") 'delete-window)
(define-key my-keys-map (kbd "C-x 4 C-0") 'kill-buffer-and-window)

(defun kill-buffer-and-frame ()
  "Kills the current buffer, if successful then delete the frame."
  (interactive)
  (when (and
         (buffer-modified-p)
         (y-or-n-p "Current buffer is modified, save?"))
    (save-buffer))
  (when (kill-buffer)
    (delete-frame)))

;;TODO: doesn't work in emacs 25
(defun delete-other-visible-frames (&optional frame)
  "Delete all frames on FRAME's terminal, except FRAME.
If FRAME uses another frame's minibuffer, the minibuffer frame is
left untouched.  Do not delete any of FRAME's child frames.  If
FRAME is a child frame, delete its siblings only.  FRAME must be
a live frame and defaults to the selected one.

Only does all of this on visible frames (might not always work)"
  (interactive)
  (setq frame (window-normalize-frame frame))
  (let ((minibuffer-frame (window-frame (minibuffer-window frame)))
        (this (next-frame frame 'visible))
        (parent (frame-parent frame))
        next)
    ;; In a first round consider minibuffer-less frames only.
    (while (not (eq this frame))
      (setq next (next-frame this 'visible))
      (unless (or (eq (window-frame (minibuffer-window this)) this)
                  ;; When FRAME is a child frame, delete its siblings
                  ;; only.
                  (and parent (not (eq (frame-parent this) parent)))
                  ;; Do not delete a child frame of FRAME.
                  (eq (frame-parent this) frame))
        (delete-frame this))
      (setq this next))
    ;; In a second round consider all remaining frames.
    (setq this (next-frame frame 'visible))
    (while (not (eq this frame))
      (setq next (next-frame this 'visible))
      (unless (or (eq this minibuffer-frame)
                  ;; When FRAME is a child frame, delete its siblings
                  ;; only.
                  (and parent (not (eq (frame-parent this) parent)))
                  ;; Do not delete a child frame of FRAME.
                  (eq (frame-parent this) frame))
        (delete-frame this))
      (setq this next))))

;; (define-key my-keys-map (kbd "M-<left>") 'winner-undo)
;; (define-key my-keys-map (kbd "M-<right>") 'winner-redo)

;;(setq framemove-hook-into-windmove t)
;;;; evil
;;;;; setup
(setq evil-emacs-state-modes nil)
(setq evil-motion-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-normal-state-modes '(prog-mode))

(setq evil-buffer-regexps (append
                           '(("^COMMIT_EDITMSG$" . insert)
                             ("^timemachine:" . emacs))
                           evil-buffer-regexps))

(add-hook 'magit-blame-mode-hook 'evil-emacs-state)

(setq evil-emacs-state-cursor '(hollow))

(evil-indent-plus-default-bindings) ;;ii iI ai aI iJ aJ

(define-key evil-emacs-state-map [escape] 'evil-normal-state)

;; (defmacro eriks/look-in-global-map (key)
;;   `(lambda ()
;;      (interactive)
;;      (call-interactively (lookup-key global-map ,key))))

;; (evil-define-key 'emacs my-keys-map
;;   [escape] 'evil-normal-state
;;   (kbd "C-w") 'evil-delete-backward-word
;;   (kbd "C-e") 'evil-scroll-line-down
;;   (kbd "C-y") 'evil-scroll-line-up
;;   (kbd "C-d") 'golden-ratio-scroll-screen-up
;;   (kbd "C-u") 'golden-ratio-scroll-screen-down
;;   (kbd "C-p") (eriks/look-in-global-map (kbd "C-y"))
;;   )

(evil-set-initial-state 'dired-mode 'emacs)

(define-key evil-normal-state-map (kbd "SPC :") 'eval-expression)
(define-key evil-normal-state-map (kbd "SPC ;") 'set-variable)
(define-key evil-normal-state-map (kbd "SPC x") 'eriks/run-bc-on-region)
(define-key evil-normal-state-map (kbd "SPC X") 'eriks/run-bc-on-region-fixed)

(define-key evil-normal-state-map (kbd "C-d") 'golden-ratio-scroll-screen-up)
(define-key evil-normal-state-map (kbd "C-u") 'golden-ratio-scroll-screen-down)
(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

(defvar eriks/bc-scale 5 "The default scale value to use in `eriks/run-bc-on-region'")

;; (defun eriks/bc-set-default-scale (scale)
;;   "Changes the default global value of scale that is used in
;; `eriks/run-bc-on-region'"
;;   (interactive "Nscale=")
;;   (setq eriks/bc-scale scale))

(defun eriks/run-bc-on-region (arg beg end &optional fixed)
  "Evaluates the region as a command to bc and replaces it with the
result.

example:
  region: ibase=16; FF
  after: 255

  region: 3+3
  after: 6

Math functions:
  s (x):   The sine of x, x is in radians.
  c (x):   The cosine of x, x is in radians.
  a (x):   The arctangent of x, arctangent returns radians.
  l (x):   The natural logarithm of x.
  e (x):   The exponential function of raising e to the value x.
  j (n,x): The bessel function of integer order n of x.

If fixed is t, then truncate the result to the value of scale.
"
  (interactive "P\nr")
  (when (region-active-p)
    (let* ((s (delete-and-extract-region beg end))
           (has-newline (equal (substring s -1 nil) "\n"))
           shell-res
           (scale (or (and arg (prefix-numeric-value arg)) eriks/bc-scale 0)))
      (when has-newline
        (setq s (substring s 0 -1)))
      (setq shell-res (replace-regexp-in-string "\\\\\n" "" (shell-command-to-string (format "echo \"scale=%s; %s%s%s\" | bc -l" scale (if fixed "(" "") s (if fixed ") / 1" "")))))
      (unless has-newline
        (setq shell-res (substring shell-res 0 -1)))
      (insert shell-res)
      (setq eriks/bc-scale scale))))

(defun eriks/run-bc-on-region-fixed (arg beg end)
  (interactive "P\nr")
  (eriks/run-bc-on-region arg beg end t))

(defun evil-what-cursor (&optional arg)
  "Extension of `what-cursor-position' that also shows how the
character can be inserted (if possible) with `evil-insert-digraph'"
  (interactive "P")
  (message "")
  (what-cursor-position arg)
  (let ((s (find
            (following-char)
            (append evil-digraphs-table-user evil-digraphs-table)
            :key #'cdr)))
    (when s
      (princ (format "%s C-k %c%c => %c" (or (current-message) ":(") (caar s) (cadar s) (cdr s))))))

(define-key evil-normal-state-map (kbd "g8") 'evil-what-cursor)

;;;;; evil remap
(defun evil-remap (trigger action &optional map)
  "remaps a key sequence to execute another key sequence in evil-mode.

'trigger' is the key sequence to \"remap\" to the key sequence 'action'.
The map to bind 'trigger' in is by default `evil-normal-state-map' (evil normal mode).

(evil-remap \"C-@\" \"@@\") will in normal mode, make C-@ virtually press @@ and run the latest keyboard macro (default behaviour). "
  (let ((mmap (if (null map) evil-normal-state-map map)))
    (define-key mmap (kbd trigger)
      `(lambda ()
         (interactive)
         (execute-kbd-macro ,action)))))

;;;;; normal mode
;;(define-key evil-normal-state-map (kbd "RET") 'newline-without-break-down)
;;(define-key evil-normal-state-map (kbd "<S-return>") 'newline-without-break-up)

(define-key evil-normal-state-map (kbd "M-k") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "M-j") 'drag-stuff-down)
(define-key evil-normal-state-map (kbd "M-h") 'drag-stuff-left)
(define-key evil-normal-state-map (kbd "M-l") 'drag-stuff-right)
(define-key evil-visual-state-map (kbd "M-k") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "M-j") 'drag-stuff-down)
(define-key evil-visual-state-map (kbd "M-h") 'drag-stuff-left)
(define-key evil-visual-state-map (kbd "M-l") 'drag-stuff-right)
;; (define-key evil-normal-state-map (kbd "C-k") 'move-text-up)
;; (define-key evil-normal-state-map (kbd "C-j") 'move-text-down)
;; (define-key evil-visual-state-map (kbd "C-j") (concat ":m '>+1" (kbd "RET") "gv=gv"))
;; (define-key evil-visual-state-map (kbd "C-k") (concat ":m '<-2" (kbd "RET") "gv=gv"))

(define-key evil-normal-state-map (kbd "<backspace>") 'evil-ex-nohighlight)

(evil-remap "C-@" "@@")

(require 'evil-latex-textobjects)

(defmacro evil-define-inner-local-textobject (key func)
  "binds key to text object func buffer-locally"
  `(progn
     (define-key evil-visual-state-local-map (kbd ,(concat "i " key)) ,func)
     (define-key evil-operator-state-local-map (kbd ,(concat "i " key)) ,func)))

(defmacro evil-define-outer-local-textobject (key func)
  "binds key to text object func buffer-locally"
  `(progn
     (define-key evil-visual-state-local-map (kbd ,(concat "a " key)) ,func)
     (define-key evil-operator-state-local-map (kbd ,(concat "a " key)) ,func)))

(defun evil-capitalize-last-word ()
  (interactive)
  (save-excursion
    ;; (evil-backward-word-end)
    (let ((end (point)))
      (evil-backward-word-begin)
      (eriks-evil-capitalize-operator (point) end))))

(define-key evil-insert-state-map (kbd "M-c") 'evil-capitalize-last-word)
(define-key my-keys-map (kbd "M-c") 'evil-capitalize-last-word)

(defvar evil-open-line-modes '((haskell-mode . same-as-prev)
                               (prog-mode . according-to-mode))
  "Settings for `evil-open-line'.
Association list of the type (mode . action) where 'action' is done
if we are in major-mode 'mode'

'action' can be either:
  same-as-prev      = same indentation as the previous line
  according-to-mode = call `indent-according-to-mode'")

(defun evil-open-line (ARG)
  "open-line for evil, designed to be the opposite of J (join-lines).

Indents the new line if it is not empty.
 - if prefix argument is a non-negative number, then indent that much
 - if prefix argument is raw, then invert the action from the following steps
 - if there is a match in `evil-open-line-modes', use that
 - else indent to the same amount as the previous line

Assumes `left-margin' is 0 or that there is no fill prefix (that
open-line doesn't indent the new line in any way)"
  (interactive "P")
  (let ((start-ind (current-indentation))
        (raw (equal ARG '(4)))
        method)
    (just-one-space 0)
    (open-line 1)
    (save-excursion
      (forward-char)
      (unless (eolp)
        (if (and (numberp ARG) (>= ARG 0))
            (indent-to ARG)
          (setq method (or (cdr (find-if #'derived-mode-p
                                         evil-open-line-modes
                                         :key 'car))
                           'same-as-prev))
          (if raw (setq method (if (eq method 'same-as-prev) 'according-to-mode 'same-as-prev)))
          (cond ((eq method 'according-to-mode)
                 (indent-according-to-mode))
                ((eq method 'same-as-prev)
                 (indent-to start-ind))))))))

(defun evil-open-line-above (ARG)
  "same as `evil-open-line' except that it is more like gO<esc>"
  (interactive "p")
  (if (bolp)
      (newline ARG)
    (save-excursion
      (beginning-of-line)
      (newline ARG))))

(defun evil-open-line-below (ARG)
  "same as `evil-open-line' except that it is more like go<esc>"
  (interactive "p")
  (save-excursion
    (end-of-line)
    (newline ARG)))

(defun evil-open-line-below-comment ()
  (interactive)
  (end-of-line)
  (indent-new-comment-line)
  (evil-insert 1))

(defun evil-open-line-above-comment ()
  (interactive)
  (evil-open-line-below-comment)
  (transpose-lines 1)
  (forward-line -2)
  (end-of-line)
  (evil-insert 1))

;;opposite to J (join-lines)
(define-key evil-normal-state-map (kbd "S") 'evil-open-line)
(define-key evil-normal-state-map (kbd "<return>")   'evil-open-line-below)
(define-key evil-normal-state-map (kbd "S-<return>") 'evil-open-line-above)
;; (define-key evil-normal-state-map (kbd "gO") 'evil-open-line-above)
;; (define-key evil-normal-state-map (kbd "go") 'evil-open-line-below)
(define-key evil-normal-state-map (kbd "SPC o") 'evil-open-line-below-comment)
(define-key evil-normal-state-map (kbd "SPC O") 'evil-open-line-above-comment)

(define-key evil-normal-state-map (kbd "C-M-j") 'indent-new-comment-line)

;; copy of the normal evil-join
(evil-define-operator eriks/evil-join-no-space (beg end)
  "Join the selected lines and remove all whitespace"
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (var count)
      (join-line 1)
      (just-one-space 0))))

(define-key evil-normal-state-map (kbd "SPC J") 'eriks/evil-join-no-space)
(define-key evil-visual-state-map (kbd "SPC J") 'eriks/evil-join-no-space)
(define-key evil-visual-state-map (kbd "J") 'evil-join)

(define-key evil-normal-state-map (kbd "K") 'noop)
(define-key evil-visual-state-map (kbd "K") 'noop)

(define-key evil-normal-state-map (kbd "ga") 'evil-lion-left)
(define-key evil-normal-state-map (kbd "gA") 'evil-lion-right)

(define-key evil-normal-state-map (kbd "U")   'undo-tree-redo)

(define-key evil-normal-state-map (kbd "C-M--") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-M-+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "M--") 'rotate-text-backward)
(define-key evil-normal-state-map (kbd "M-+") 'rotate-text)

;; (define-key evil-normal-state-map (kbd "gtc") 'transpose-chars)
;; (define-key evil-normal-state-map (kbd "gtl") 'transpose-lines)
;; (define-key evil-normal-state-map (kbd "gtw") 'transpose-words)
;; (define-key evil-normal-state-map (kbd "gts") 'subword-transpose)
;; (define-key evil-normal-state-map (kbd "gt C-l") 'subword-transpose)
;; (define-key evil-normal-state-map (kbd "gtr") 'eriks-region-switch)
;; (define-key evil-normal-state-map (kbd "gtR") 'eriks-region-switch-abort)

;;;;; nerd commenter
(require 'evil-nerd-commenter)

(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
(define-key evil-normal-state-map (kbd "gC") 'evilnc-copy-and-comment-operator)

(define-key evil-inner-text-objects-map (kbd "c") 'evilnc-inner-comment)
(define-key evil-outer-text-objects-map (kbd "c") 'evilnc-outer-commenter)

;;;;; extra operator
(require 'evil-extra-operator)

(define-key evil-normal-state-map (kbd "gr") 'evil-operator-eval)

(defun search-online (website search)
  (interactive "sWebsite: \nsQuery: ")
  (browse-url
   (concat website (url-hexify-string search))))

(defun search-online-google (search)
  (interactive "sSearch: ")
  (search-online "http://google.com/search?q=" search))

(define-key evil-normal-state-map (kbd "SPC s G") 'search-online-google)

(defun search-online-javadoc (search)
  (interactive "sSearch: ")
  (search-online "http://javadocs.org/" search))

(define-key evil-normal-state-map (kbd "SPC s J") 'search-online-javadoc)

(evil-define-operator eriks-evil-javadoc-search-operator (beg end type)
  "Evil operator for javadoc search."
  :move-point nil
  (interactive "<R>")
  (browse-url
   (concat "http://javadocs.org/"
           (url-hexify-string
            (.eeo/make-url-args beg end type)))))

(define-key evil-normal-state-map (kbd "SPC s g") 'evil-operator-google-search)
(define-key evil-normal-state-map (kbd "SPC s j") 'eriks-evil-javadoc-search-operator)
(define-key evil-visual-state-map (kbd "SPC s g") 'evil-operator-google-search)
(define-key evil-visual-state-map (kbd "SPC s j") 'eriks-evil-javadoc-search-operator)

(evil-define-operator eriks-evil-swiper-operator (beg end type)
  (interactive "<R>")
  (when (evil-visual-state-p)
    (evil-exit-visual-state))
  (swiper (buffer-substring beg end)))

(evil-define-operator eriks-evil-capitalize-operator (beg end type)
  (interactive "<R>")
  (when (evil-visual-state-p)
    (evil-exit-visual-state))
  (capitalize-region beg end))

(define-key evil-normal-state-map (kbd "g C-u") 'eriks-evil-capitalize-operator)
(define-key evil-visual-state-map (kbd "g C-u") 'eriks-evil-capitalize-operator)
(define-key evil-visual-state-map (kbd "M-c") 'eriks-evil-capitalize-operator)
(define-key evil-normal-state-map (kbd "M-c") 'eriks-evil-capitalize-operator)

;; (define-key evil-normal-state-map (kbd "g o") 'eriks-evil-capitalize-operator)
;; (define-key evil-visual-state-map (kbd "g o") 'eriks-evil-capitalize-operator)

(define-key evil-normal-state-map (kbd "SPC s s") 'eriks-evil-swiper-operator)
(define-key evil-visual-state-map (kbd "SPC s s") 'eriks-evil-swiper-operator)

(define-key evil-normal-state-map (kbd "SPC s S") 'swiper)

(define-key evil-normal-state-map (kbd "gp") 'evil-operator-clone)
(define-key evil-visual-state-map (kbd "gp") 'evil-operator-clone)

(require 'evil-little-word)
(define-key evil-motion-state-map (kbd "C-l C-w") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "C-l C-b") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "C-l C-S-w") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "C-l C-e") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "C-l C-S-b") 'evil-backward-little-word-end)
(define-key evil-motion-state-map (kbd "C-l w") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "C-l b") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "C-l W") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "C-l e") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "C-l B") 'evil-backward-little-word-end)
(define-key evil-outer-text-objects-map (kbd "C-l") 'evil-a-little-word)
(define-key evil-inner-text-objects-map (kbd "C-l") 'evil-inner-little-word)

(define-key evil-motion-state-map (kbd "H-w") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "H-b") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "H-e") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "H-B") 'evil-backward-little-word-end)
(define-key evil-outer-text-objects-map (kbd "H-l") 'evil-a-little-word)
(define-key evil-inner-text-objects-map (kbd "H-l") 'evil-inner-little-word)

(define-key evil-normal-state-map (kbd "SPC s M") 'dmoccur)
(define-key evil-normal-state-map (kbd "SPC s N") 'moccur)
(define-key evil-normal-state-map (kbd "SPC s O") 'occur-by-moccur)

(define-key evil-normal-state-map (kbd "SPC s f") 'find-dired)

(define-key evil-normal-state-map (kbd "SPC s a p") 'projectile-ag)
(define-key evil-normal-state-map (kbd "SPC s a c") 'counsel-ag)
(define-key evil-normal-state-map (kbd "SPC s a a") 'ag)
(define-key evil-normal-state-map (kbd "SPC s a r") 'ag-regexp)
(define-key evil-normal-state-map (kbd "SPC s a R") 'ag-project-regexp)
(define-key evil-normal-state-map (kbd "SPC s a P") 'ag-project)

(setq evil-operator-moccur-grep-find-key (kbd "SPC s m"))
(require 'evil-operator-moccur)
(global-evil-operator-moccur-mode 1)

;;;;; evil-highlight
(require 'hi-lock)

(setq-default hi-lock-face-defaults (list "hi-yellow"
                                          "hi-pink"
                                          "hi-blue"
                                          "hi-red"
                                          "hi-cyan"
                                          "hi-yellow-p"
                                          "hi-pink-p"
                                          "hi-blue-p"
                                          "hi-red-p"
                                          "hi-cyan-p"))

(defun eriks-evil-highlight (reg &optional AUTO-SELECT-FACE)
  (let* ((hi-lock-auto-select-face AUTO-SELECT-FACE)
         (current-prefix-arg nil)
         (face (hi-lock-read-face-name)))
    (hi-lock-face-buffer reg face)))

(evil-define-operator eriks-evil-highlight-operator (beg end type)
  (interactive "<R>")
  (eriks-evil-highlight (regexp-quote (buffer-substring beg end)) t))

(evil-define-operator eriks-evil-highlight-operator-manual (beg end type)
  (interactive "<R>")
  (eriks-evil-highlight (regexp-quote (buffer-substring beg end)) nil))

(defun eriks-evil-highlight-regex (ARG)
  "highlights a given regex written in minibuffer interactively.
Uses a default face unless C-u is used."
  (interactive "P")
  (eriks-evil-highlight
   (read-regexp "highlight" "" 'evil-ex-search-history)
   (null (equal ARG '(4)))))

(define-key evil-normal-state-map (kbd "SPC h h") 'eriks-evil-highlight-operator)        ;; highlight motion,     auto face
(define-key evil-visual-state-map (kbd "SPC h h") 'eriks-evil-highlight-operator)        ;; highlight selection,  auto face
(define-key evil-normal-state-map (kbd "SPC h H") 'eriks-evil-highlight-operator-manual) ;; highlight motion,     manual face
(define-key evil-visual-state-map (kbd "SPC h H") 'eriks-evil-highlight-operator-manual) ;; highlight selection,  manual face
(define-key evil-normal-state-map (kbd "SPC h u") 'unhighlight-regexp)                   ;; unhighlight prompted, all with C-u
(define-key evil-normal-state-map (kbd "SPC h r") 'eriks-evil-highlight-regex)           ;; highlight prompted,   auto face unless C-u

;;;;; evil surround
(require 'evil-surround)
(evil-surround-install-text-objects)
(global-evil-surround-mode 1)

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-Surround-region)
(evil-define-key 'normal evil-surround-mode-map "gs" 'evil-surround-edit)
(evil-define-key 'normal evil-surround-mode-map "gS" 'evil-Surround-edit)

;;;;; evil collection typ
;; (setq evil-want-integration nil)
;; (require 'evil-collection)
;; (require 'evil-collection-calc)
;; (evil-collection-calc-setup)

;;;;;; outline minor mode
(evil-define-key '(normal visual motion) outline-minor-mode-map
  (kbd "zo")  'outline-show-more
  (kbd "zp")  'outline-hide-other
  (kbd "zB")  'outline-hide-body
  (kbd "zb")  'outline-hide-entry
  (kbd "ze")  'outline-show-entry
  (kbd "zl")  'outline-hide-leaves
  (kbd "zk")  'outline-show-children
  (kbd "zK")  'outline-show-branches
  (kbd "zu")  'outline-up-heading

  (kbd "zxn") 'outshine-narrow-to-subtree
  (kbd "zxw") 'widen

  (kbd "zxj") 'outline-forward-same-level
  (kbd "zxk") 'outline-backward-same-level
  (kbd "zxl") 'outline-next-visible-heading
  (kbd "zxh") 'outline-previous-visible-heading

  (kbd "zxJ") 'outline-move-subtree-down
  (kbd "zxK") 'outline-move-subtree-up
  (kbd "zxH") 'outline-promote
  (kbd "zxL") 'outline-demote

  (kbd "zxi") 'outshine-insert-heading
  (kbd "zxc") 'outshine-cycle-buffer
  ;; (kbd "zxf") 'outline-hide-entry
  ;; (kbd "zxs") 'outline-show-entry
  )

;;;;; visual mode
;; (define-key evil-visual-state-map (kbd "gx") 'exchange-point-and-mark)

;; (evil-remap "g C-n" "\\rN1%d" evil-visual-state-map)

(defun eriks-evil-visual-block-insert (start padding format)
  (save-excursion
    (evil-emacs-state nil)
    (rectangle-number-lines
     (region-beginning)
     (region-end)
     start
     (format format
             (cond
              ((= padding 0)
               "")
              (t
               (number-to-string (- 0 padding))))))
    (evil-normal-state)))

(defun eriks-evil-visual-block-insert-numbers-zero (padding)
  (interactive "p")
  (eriks-evil-visual-block-insert 1 padding "%%0%sd"))

(defun eriks-evil-visual-block-insert-numbers (padding)
  (interactive "p")
  (eriks-evil-visual-block-insert 1 padding "%%%sd"))

(defun eriks-evil-visual-block-insert-letters (padding)
  (interactive "p")
  (eriks-evil-visual-block-insert 97 padding "%%%sc"))

(defun eriks-evil-visual-block-insert-alt ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'rectangle-number-lines))

(define-key evil-visual-state-map (kbd "SPC i n") 'eriks-evil-visual-block-insert-numbers)
(define-key evil-visual-state-map (kbd "SPC i N") 'eriks-evil-visual-block-insert-numbers-zero)
(define-key evil-visual-state-map (kbd "SPC i l") 'eriks-evil-visual-block-insert-letters)
(define-key evil-visual-state-map (kbd "SPC i i") 'eriks-evil-visual-block-insert-alt)

;;;;; space key
;; (define-prefix-command 'evil-spc)
;; (define-key evil-normal-state-map (kbd "SPC") 'evil-spc)
;;(define-key evil-visual-state-map (kbd "SPC") 'evil-spc)

;; (define-key evil-normal-state-map (kbd "SPC M-+") 'hydra-evil-numbers/body)

;;(define-key evil-spc (kbd ";") 'comment-dwim)

;; (define-key evil-normal-state-map (kbd "SPC x") 'eriks/delete-empty-parens)

(define-key evil-normal-state-map (kbd "SPC u") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "SPC .") 'repeat)

(define-key evil-normal-state-map (kbd "SPC p]") 'sp-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd "SPC p[") 'sp-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd "SPC p}") 'sp-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "SPC p{") 'sp-backward-barf-sexp)
(define-key evil-normal-state-map (kbd "SPC ps") 'sp-split-sexp)
(define-key evil-normal-state-map (kbd "SPC pj") 'sp-join-sexp)
(define-key evil-normal-state-map (kbd "SPC pt") 'sp-transpose-sexp)
(define-key evil-normal-state-map (kbd "SPC pc") 'sp-convolute-sexp)
(define-key evil-normal-state-map (kbd "SPC pb") 'sp-extract-before-sexp)
(define-key evil-normal-state-map (kbd "SPC pa") 'sp-extract-after-sexp)
(define-key evil-normal-state-map (kbd "SPC pu") 'sp-splice-sexp)
(define-key evil-normal-state-map (kbd "SPC pk") 'sp-kill-sexp)
(define-key evil-normal-state-map (kbd "SPC pd") 'sp-splice-sexp-killing-around)
(define-key evil-normal-state-map (kbd "SPC pl") 'sp-move-sexp-forward)
(define-key evil-normal-state-map (kbd "SPC ph") 'sp-move-sexp-back)

(define-key evil-normal-state-map (kbd "H-]") 'sp-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd "H-[") 'sp-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd "H-}") 'sp-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "H-{") 'sp-backward-barf-sexp)
;; (define-key evil-normal-state-map (kbd "H-s") 'sp-split-sexp)
;; (define-key evil-normal-state-map (kbd "H-i") 'sp-join-sexp)
(define-key evil-normal-state-map (kbd "H-k") 'sp-move-sexp-back)
(define-key evil-normal-state-map (kbd "H-j") 'sp-move-sexp-forward)
;; (define-key evil-normal-state-map (kbd "H-a") 'sp-add-to-next-sexp)
;; (define-key evil-normal-state-map (kbd "H-b") 'sp-extract-before-sexp)
;; (define-key evil-normal-state-map (kbd "H-c") 'sp-convolute-sexp)
;; (define-key evil-normal-state-map (kbd "H-f") 'sp-extract-after-sexp)
(define-key evil-normal-state-map (kbd "H-u") 'sp-splice-sexp)
;; (define-key evil-normal-state-map (kbd "H-k") 'sp-kill-sexp)
(define-key evil-normal-state-map (kbd "H-d") 'sp-splice-sexp-killing-around)

;; ARG is most likely not supported
(defun sp-move-sexp-back (&optional ARG)
  "Moves the current sexp backward. Point needs to be on the most left
side of the sexp"
  (interactive "p")
  (let ((p (point)))
    (save-excursion
      (sp-backward-parallel-sexp ARG)
      (setq p (< (point) p)))
    (when p
      (sp-forward-sexp)
      (sp-transpose-sexp (- (or ARG 1)))
      (sp-backward-sexp))))

(defun sp-move-sexp-forward (&optional ARG)
  "Moves the current sexp forward. Point needs to be on the most left
side of the sexp"
  (interactive "p")
  (let ((q (point))
        p)
    (sp-forward-sexp)
    (setq p (point))
    (save-excursion
      (sp-forward-parallel-sexp)
      (setq p (> (point) p)))
    (if p
        (progn
          (sp-transpose-sexp)
          (sp-backward-sexp))
      (goto-char q))))

(define-key evil-normal-state-map (kbd "SPC Y") 'browse-kill-ring)
(define-key evil-normal-state-map (kbd "SPC y") 'counsel-yank-pop)

;;;;; insert motion
(define-key evil-insert-state-map (kbd "C-^") 'sp-up-sexp)
(define-key evil-insert-state-map (kbd "C-u") 'sp-up-sexp)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

(define-key evil-normal-state-map (kbd "C-a") 'eriks/line-cleanup-dwim)
(define-key evil-insert-state-map (kbd "C-a") 'eriks/line-cleanup-dwim)

(define-key evil-insert-state-map (kbd "C-s") 'eriks/delete-empty-parens)
;; (define-key evil-normal-state-map (kbd "C-k") 'eriks/delete-empty-parens)

(defun eriks/delete-trailing-this-line ()
  "Removes trailing whitespace from the current line."
  (interactive)
  (let ((l (bounds-of-thing-at-point 'line)))
    (save-excursion
      (goto-char (1- (cdr l)))
      (while (and
              (>= (point) (car l))
              (is-whitespace (following-char) nil))
        (when (is-whitespace (following-char) t)
          (delete-char 1))
        (forward-char -1)))))

(defun eriks/line-cleanup-dwim ()
  " - if the mark is active, then remove trailing whitespace inside region
 - if the line is empty, then indent it to the same level as the previous line
 - if the line is not empty and only contains indentation, then delete the line
 - if the line is not empty and contains non-indentation characters, then remove trailing whitespace"
  (interactive)
  (if (use-region-p)
      (progn
        (delete-trailing-whitespace (region-beginning) (region-end))
        (deactivate-mark))
    (let* ((l (bounds-of-thing-at-point 'line))
           (i (current-indentation))
           (non-i (- (- (cdr l) (car l)) i 1)))
      (cond
       ((or (and (> i 0) (<= non-i 0)) ; only indentation
            (> non-i 0))              ; contains non-indentation
        (eriks/delete-trailing-this-line))
       ((and (= i 0) (= non-i 0))     ; completely empty line
        (save-excursion
          (when (= 0 (forward-line -1))
            (setq i (current-indentation))))
        (indent-to i))))))

;;;;; space motion
(require 'evil-easymotion)
(evilem-default-keybindings "SPC")

(defun eriks/avy--regex-candidates-exclusive (regex &optional beg end)
  "same as `avy--regex-candidates' except that all candidates are
  before the actual candidate. Only makes sense/works on the current
  window where the point is."
  (let ((p (point))
        (cur-window (selected-window)))
    (mapcar
     (lambda (a)
       (pcase a
         ((and
           `((,b . ,e) . ,w)
           (guard (equal w cur-window)))
          (cond ((> b p)
                 `((,(1- b) . ,(1- e)) . ,w))
                ((< e p)
                 `((,(1+ b) . ,(1+ e)) . ,w))
                (t
                 `((,b . ,e) . ,w))))
         (_ a)))
     (avy--regex-candidates regex beg end))))

(defun eriks/avy-goto-char-in-line-exclusive (char)
  "Same as `avy-goto-char-in-line' except that it doesn't include the
target character"
  (interactive "cchar:")
  (avy--process
   (eriks/avy--regex-candidates-exclusive
    (regexp-quote (string char))
    (line-beginning-position)
    (line-end-position))
   (avy--style-fn avy-style)))

(evil-define-avy-motion eriks/avy-goto-char-in-line-exclusive inclusive)
(define-key evil-motion-state-map (kbd ".") 'evil-eriks/avy-goto-char-in-line-exclusive)

(define-key evil-motion-state-map (kbd ",") 'avy-goto-char-in-line)
(define-key evil-motion-state-map (kbd "SPC ,") 'avy-goto-char)

(define-key evil-motion-state-map (kbd ";") 'avy-goto-char-timer)

(define-key evil-motion-state-map (kbd "+") 'evilem-motion-next-line-first-non-blank)
(define-key evil-motion-state-map (kbd "-") 'evilem-motion-previous-line-first-non-blank)
(define-key evil-motion-state-map (kbd "J") 'evilem-motion-next-line-first-non-blank)
(define-key evil-motion-state-map (kbd "K") 'evilem-motion-previous-line-first-non-blank)

(define-key evil-motion-state-map (kbd "C-;") 'evil-repeat-find-char)
(define-key evil-motion-state-map (kbd "C-,") 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map (kbd "H-.") 'evil-repeat-find-char)
(define-key evil-motion-state-map (kbd "H-,") 'evil-repeat-find-char-reverse)

(evilem-define (kbd "SPC ][") 'sp-next-sexp)
(evilem-define (kbd "SPC ]]") 'sp-forward-sexp)
(evilem-define (kbd "SPC [[") 'sp-backward-sexp)
(evilem-define (kbd "SPC []") 'sp-previous-sexp)

(evilem-define (kbd "SPC {") 'sp-backward-up-sexp)
(evilem-define (kbd "SPC }") 'sp-up-sexp)
(evilem-define (kbd "SPC M-[") 'sp-backward-down-sexp)
(evilem-define (kbd "SPC M-]") 'sp-down-sexp)
(evilem-define (kbd "SPC (") 'sp-beginning-of-sexp)
(evilem-define (kbd "SPC )") 'sp-end-of-sexp)

(evilem-define (kbd "SPC ]s") 'forward-sentence)
(evilem-define (kbd "SPC [s") 'backward-sentence)
(evilem-define (kbd "SPC ]p") 'forward-paragraph)
(evilem-define (kbd "SPC [p") 'backward-paragraph)

(evilem-define (kbd "SPC [i") 'eriks-up-indentation)

;;;;; motion map
(define-key evil-motion-state-map (kbd "[m") 'evil-backward-section-begin)
(define-key evil-motion-state-map (kbd "[M") 'evil-backward-section-end)
(define-key evil-motion-state-map (kbd "]m") 'evil-forward-section-begin)
(define-key evil-motion-state-map (kbd "]M") 'evil-forward-section-end)

(evil-remap "[<" "F<" evil-motion-state-map)
(evil-remap "[>" "F>" evil-motion-state-map)
(evil-remap "]<" "f<" evil-motion-state-map)
(evil-remap "]>" "f>" evil-motion-state-map)

(define-key evil-motion-state-map (kbd "]s") 'forward-sentence)
(define-key evil-motion-state-map (kbd "[s") 'backward-sentence)
(define-key evil-motion-state-map (kbd "]p") 'forward-paragraph)
(define-key evil-motion-state-map (kbd "[p") 'backward-paragraph)

(define-key evil-motion-state-map (kbd "][") 'sp-next-sexp)
(define-key evil-motion-state-map (kbd "]]") 'sp-forward-sexp) ;; sp-forward-parallel-sexp
(define-key evil-motion-state-map (kbd "[[") 'sp-backward-sexp)
(define-key evil-motion-state-map (kbd "[]") 'sp-previous-sexp)

(define-key evil-motion-state-map (kbd "[d") 'beginning-of-defun)
(define-key evil-motion-state-map (kbd "]d") 'end-of-defun)

(define-key evil-motion-state-map (kbd "[i") 'eriks-up-indentation)

(define-key evil-motion-state-map (kbd "[c") 'eriks-up-same-column)
(define-key evil-motion-state-map (kbd "]c") 'eriks-down-same-column)

(define-key evil-motion-state-map (kbd "[ SPC") 'eriks-skip-space-backwards)
(define-key evil-motion-state-map (kbd "] SPC") 'eriks-skip-space)

(define-key evil-motion-state-map (kbd "{") 'sp-backward-up-sexp)
(define-key evil-motion-state-map (kbd "}") 'sp-up-sexp)
(define-key evil-motion-state-map (kbd "M-[") 'sp-backward-down-sexp)
(define-key evil-motion-state-map (kbd "M-]") 'sp-down-sexp)
(define-key evil-motion-state-map (kbd "(") 'sp-beginning-of-sexp)
(define-key evil-motion-state-map (kbd ")") 'sp-end-of-sexp)

;;;;; evil-exchange
(require 'evil-exchange)
(evil-exchange-install)

;;;;; multiple cursors
;; yank fix
;; https://github.com/gabesoft/evil-mc/issues/70
(add-hook 'evil-mc-after-cursors-deleted
          (lambda ()
            (setq evil-was-yanked-without-register t)))

(defvar evil-mc-key-map
  (let ((map (make-sparse-keymap))
        (keys '(("gmm"   . evil-mc-make-all-cursors)
                ("gmu"   . evil-mc-undo-all-cursors)
                ("gms"   . evil-mc-pause-cursors)
                ("gmr"   . evil-mc-resume-cursors)
                ("gmf"   . evil-mc-make-and-goto-first-cursor)
                ("gml"   . evil-mc-make-and-goto-last-cursor)
                ("gmh"   . evil-mc-make-cursor-here)
                ("gmj"   . evil-mc-make-cursor-move-next-line)
                ("gmk"   . evil-mc-make-cursor-move-prev-line)
                ("C-S-n" . evil-mc-make-cursor-move-next-line)
                ("C-S-p" . evil-mc-make-cursor-move-prev-line)
                ("gmN"   . evil-mc-skip-and-goto-next-cursor)
                ("gmP"   . evil-mc-skip-and-goto-prev-cursor)
                ("gmn"   . evil-mc-skip-and-goto-next-match)
                ("gmp"   . evil-mc-skip-and-goto-prev-match)
                ("M-n"   . evil-mc-make-and-goto-next-cursor)
                ("M-p"   . evil-mc-make-and-goto-prev-cursor)
                ("C-n"   . evil-mc-make-and-goto-next-match)
                ("C-p"   . evil-mc-make-and-goto-prev-match)
                ("gm+"   . evil-mc-inc-num-at-each-cursor)
                ("gm-"   . evil-mc-dec-num-at-each-cursor)
                ;; ("C-S-t" . evil-mc-skip-and-goto-next-match)
                )))
    (dolist (key-data keys)
      (evil-define-key 'normal map (kbd (car key-data)) (cdr key-data))
      (evil-define-key 'visual map (kbd (car key-data)) (cdr key-data)))
    map))

(setq evil-mc-one-cursor-show-mode-line-text nil)
(setq evil-mc-mode-line-text-cursor-color nil)
(setq evil-mc-mode-line-text-inverse-colors nil)
(setq evil-mc-mode-line-text-cursor-color nil)
(require 'evil-mc)
(require 'evil-mc-extras)

(add-to-list 'evil-mc-known-commands '(evil-surround-edit (:default . evil-mc-execute-default-evil-surround-region)))

(dolist (cmd '(eval-last-sexp-replace
               sp-end-of-sexp
               sp-beginning-of-sexp
               sp-up-sexp
               sp-backward-up-sexp
               sp-down-sexp
               sp-backward-down-sexp))
  (add-to-list 'evil-mc-known-commands `(,cmd (:default . ,cmd))))

;; Temporary fix for bug with change command with multiple cursors
;; https://github.com/gabesoft/evil-mc/issues/63
(add-hook 'evil-mc-before-cursors-created (lambda () (setq-default evil-move-cursor-back t)))
(add-hook 'evil-mc-after-cursors-deleted (lambda () (setq-default evil-move-cursor-back nil)))

(global-evil-mc-mode 1)

;;;;; ggtags

(define-key evil-normal-state-map (kbd "SPC SPC") 'ggtags-find-tag-dwim)
(define-key evil-normal-state-map (kbd "H-o") 'ggtags-prev-mark)
(define-key evil-normal-state-map (kbd "H-i") 'ggtags-next-mark)

;; (evil-define-key 'normal ggtags-mode-map (kbd "C-]") 'ggtags-find-tag-dwim)
;; (evil-define-key 'normal ggtags-mode-map (kbd "C-o") 'ggtags-prev-mark)
;; (evil-define-key 'normal ggtags-mode-map (kbd "C-i") 'ggtags-next-mark)

;;;;; other

(evil-define-text-object eriks-evil-inside-line-text-object (count &optional beg end type)
  (save-excursion
    (evil-first-non-blank)
    (let ((first (point)))
      (evil-end-of-line)
      (evil-range first (point)))))

(evil-define-text-object eriks-evil-outside-line-text-object (count &optional beg end type)
  (save-excursion
    (move-beginning-of-line nil)
    (let ((first (point)))
      (evil-end-of-line)
      (evil-range first (point)))))

(define-key evil-inner-text-objects-map (kbd "l") 'eriks-evil-inside-line-text-object)
(define-key evil-outer-text-objects-map (kbd "l") 'eriks-evil-outside-line-text-object)

(evil-define-text-object eriks-evil-inside-whitespace-text-object (count &optional beg end type)
  (save-excursion
    (eriks-skip-space -1 t)
    (let ((first (point)))
      (eriks-skip-space 1 t)
      (evil-range first (point)))))

(evil-define-text-object eriks-evil-outside-whitespace-text-object (count &optional beg end type)
  (save-excursion
    (eriks-skip-space -1 nil)
    (let ((first (point)))
      (eriks-skip-space 1 nil)
      (evil-range first (point)))))

(define-key evil-inner-text-objects-map (kbd "SPC") 'eriks-evil-inside-whitespace-text-object)
(define-key evil-outer-text-objects-map (kbd "SPC") 'eriks-evil-outside-whitespace-text-object)

(evil-define-text-object eriks-evil-inside-defun-text-object (count &optional beg end type)
  (save-excursion
    (beginning-of-defun)
    (let ((first (point)))
      (end-of-defun)
      (evil-range first (point)))))

(define-key evil-inner-text-objects-map (kbd "d") 'eriks-evil-inside-defun-text-object)


(evil-define-text-object erik-evil-inner-form-text-object (count &optional beg end type)
  (save-excursion
    (sp-beginning-of-sexp)
    (let ((first (point)))
      (sp-end-of-sexp)
      (evil-range first (point)))))

(evil-define-text-object erik-evil-outer-form-text-object (count &optional beg end type)
  (save-excursion
    (let ((start (point)))
      (sp-backward-up-sexp)
      (let ((first (point)))
        (goto-char start)
        (sp-up-sexp)
        (evil-range first (point))))))

(defun erik-evil-top-form-up (cursor)
  (interactive "d")
  (ignore-errors
    (sp-backward-up-sexp))
  (unless (= cursor (point))
    (erik-evil-top-form-up (point))))

(defun erik-evil-top-form-end (cursor)
  (interactive "d")
  (erik-evil-top-form-up cursor)
  (sp-forward-sexp))

(define-key evil-motion-state-map (kbd "g{") 'erik-evil-top-form-up)
(define-key evil-motion-state-map (kbd "g}") 'erik-evil-top-form-end)

(evil-define-text-object erik-evil-outer-top-form-text-object (count &optional beg end type)
  (save-excursion
    (erik-evil-top-form-up (point))
    (let ((first (point)))
      (sp-forward-sexp)
      (evil-range first (point)))))

(evil-define-text-object erik-evil-inner-top-form-text-object (count &optional beg end type)
  (save-excursion
    (erik-evil-top-form-up (point))
    (let ((first (point)))
      (sp-forward-sexp)
      (let ((end (point)))
        (goto-char first)
        (sp-down-sexp)
        (let ((actual-first (point)))
          (goto-char end)
          (sp-backward-down-sexp)
          (evil-range actual-first (point)))))))

(define-key evil-outer-text-objects-map (kbd "e") 'erik-evil-outer-form-text-object)
(define-key evil-inner-text-objects-map (kbd "e") 'erik-evil-inner-form-text-object)
(define-key evil-outer-text-objects-map (kbd "E") 'erik-evil-outer-top-form-text-object)
(define-key evil-inner-text-objects-map (kbd "E") 'erik-evil-inner-top-form-text-object)

;; (require 'evil-cleverparens-text-objects)

;; (define-key evil-inner-text-objects-map (kbd "e") 'evil-cp-inner-form)
;; (define-key evil-outer-text-objects-map (kbd "e") 'evil-cp-a-form)
;; (define-key evil-inner-text-objects-map (kbd "E") 'evil-cp-inner-defun)
;; (define-key evil-outer-text-objects-map (kbd "E") 'evil-cp-a-defun)

;; fastnar i infinite loop ibland
;; (defun eriks-down-indentation ()
;;   (interactive)
;;   (back-to-indentation)
;;   (let ((start (current-column)))
;;     (while (= (current-column) start)
;;       (forward-line 1)
;;       (back-to-indentation))))

(evil-mode 1)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; (add-to-list 'evil-surround-pairs-alist '(?f . erik-evil-surround-latex-macro))
            (add-to-list 'evil-surround-pairs-alist '(?$ . ("$" . "$")))
            (evil-define-inner-local-textobject "$" 'evil-latex-textobjects-inner-dollar)
            (evil-define-outer-local-textobject "$" 'evil-latex-textobjects-a-dollar)
            (evil-define-inner-local-textobject "\\" 'evil-latex-textobjects-inner-math)
            (evil-define-outer-local-textobject "\\" 'evil-latex-textobjects-a-math)
            (evil-define-inner-local-textobject "f" 'evil-latex-textobjects-inner-macro)
            (evil-define-outer-local-textobject "f" 'evil-latex-textobjects-a-macro)
            (evil-define-inner-local-textobject "m" 'evil-latex-textobjects-inner-env)
            (evil-define-outer-local-textobject "m" 'evil-latex-textobjects-an-env)))

;;;;; modifications

;;;;;; from evil-commands.el

;; incorrectly moved point one step too far forward
(defun evil-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or evil-this-register (read-char))))
       (delete-overlay overlay))))
  (when (evil-paste-before nil register t)
    ;; go to end of pasted text
    ;; (unless (eobp) ;; removed these
    ;;   (forward-char))
    ))

;;;;;; evil-lion.el

(require 'evil-lion)

;; improvements, can now give COUNT arger than 1
(defun evil-lion--align-region (type count beg end regex)
  "Build input for (align-region) and call it.

TYPE can be either 'left or 'right.
If COUNT is 1, the alignment will be performed on the first occurance
only.
BEG and END specify the retion to align.
REGEX is the regex to align by."
  (when (> (length regex) 0)

    ;; (when (and count (> count 1))
    ;;   (user-error "Only COUNT `1' is supported at the moment"))

    (save-restriction
      (narrow-to-region beg end)

      ;; squeeze spaces if configured to do so
      ;; (when evil-lion-squeeze-spaces
      ;;   (evil-lion--squeeze-spaces type count (point-min) (point-max) regex))

      ;; left --  (%s.*?){%d}\(\s-*\)%s
      ;; right -- (%s.*?){%d}%s\(\s-*\)

      ;; prepare input for align-region and call it
      (let* ((indent-tabs-mode nil)
             (group-regex
              (if evil-lion-squeeze-spaces "\\(\\s-*\\)" "\\(\\)"))
             (regexp
              (cond ((and (not (null count)) (> count 1))
                     (if (eq type 'left)
                         (format "\\(%s.*?\\)\\{%d\\}%s%s" regex (- count 1) group-regex regex)
                       (format "\\(%s.*?\\)\\{%d\\}%s%s" regex (- count 1) regex group-regex)))
                    (t
                     (if (eq type 'left)
                         (concat group-regex regex)
                       (concat regex group-regex)))))
             (spacing 1)
             (repeat
              (if (or (null count) (eq count 0)) t nil))
             (group (if (and (not (null count)) (> count 1)) 2 1))
             (rule
              (list (list nil (cons 'regexp regexp)
                          (cons 'group group)
                          (cons 'spacing spacing)
                          (cons 'repeat repeat)))))
        ;; if align-region isn't loaded, load it
        (unless (fboundp 'align-region)
          (require 'align))
        (align-region (point-min) (point-max) 'entire rule nil nil)))))


;;;; git-gutter
(setq git-gutter-fr+-side 'right-fringe)
(require 'git-gutter-fringe+)

(set-face-foreground 'git-gutter-fr+-modified "yellow")
(set-face-background 'git-gutter-fr+-modified "yellow")

(set-face-foreground 'git-gutter-fr+-deleted "red")
(set-face-background 'git-gutter-fr+-deleted "red")

(set-face-foreground 'git-gutter-fr+-added "green")
(set-face-background 'git-gutter-fr+-added "green")

;; (fringe-helper-define 'git-gutter-fr+-modified nil
;;   "XXXXXXXX"
;;   "XXXXXXXX"
;;   "........"
;;   "........"
;;   "........"
;;   "........"
;;   "XXXXXXXX"
;;   "XXXXXXXX"
;;   )

(global-git-gutter+-mode 1)
;; (git-gutter+-toggle-fringe)

(define-key eriks-map (kbd "gg") 'git-gutter+-mode) ; Turn on/off in the current buffer
(define-key eriks-map (kbd "gG") 'global-git-gutter+-mode) ; Turn on/off globally
(define-key eriks-map (kbd "gj") 'git-gutter+-next-hunk)
(define-key eriks-map (kbd "gk") 'git-gutter+-previous-hunk)

;;;; rotate-text
(setq rotate-text-symbols nil)

(push '("true" "false") rotate-text-words)

(defvar rotate-text-c-like-symbols
  '(("||" "&&")
    ("==" "!=")
    (">=" "<=" "<" ">")))

(defvar rotate-text-java-symbols
  '(("protected" "public" "private")))

(defmacro make-rotate-text-hook (hook-name patterns symbols words)
  `(defun ,(intern (concat "rotate-text-" (symbol-name hook-name) "-hook")) ()
     (setq rotate-text-local-symbols (apply #'append (cons rotate-text-local-symbols ,symbols)))
     (setq rotate-text-local-words (apply #'append (cons rotate-text-local-words ,words)))
     (setq rotate-text-local-patterns (apply #'append (cons rotate-text-local-patterns ,patterns)))))

(make-rotate-text-hook c nil (list rotate-text-c-like-symbols) nil)
(make-rotate-text-hook java nil (list rotate-text-java-symbols rotate-text-c-like-symbols) nil)

(add-hook 'c-mode-hook    'rotate-text-c-hook)
(add-hook 'java-mode-hook 'rotate-text-java-hook)
(add-hook 'js-mode-hook   'rotate-text-c-hook)

;;;; eyebrowse
;; (define-key eyebrowse-mode-map (kbd "C-<") 'eyebrowse-prev-window-config)
;; (define-key eyebrowse-mode-map (kbd "C->") 'eyebrowse-next-window-config)
;; (define-key eyebrowse-mode-map (kbd "C-'") 'eyebrowse-last-window-config)
;; (define-key eyebrowse-mode-map (kbd "C-\"") 'eyebrowse-close-window-config)
;; (define-key eyebrowse-mode-map (kbd "C-c C-w q") 'eyebrowse-close-window-config)
;; (define-key eyebrowse-mode-map (kbd "M-q") 'eyebrowse-close-window-config)
;; ;; (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
;; (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
;; (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
;; (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
;; (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
;; (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
;; (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
;; (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
;; (define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
;; (define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)

;; (eyebrowse-mode t)

;;;; git-timemachine
(require 'git-timemachine)
(defun eriks/git-timemachine-show-commit ()
  "Show info about the currently visiting commit"
  (interactive)
  (if (fboundp 'magit-show-commit)
      (magit-show-commit (car git-timemachine-revision))
    (message "You need to install magit for this")))

(define-key git-timemachine-mode-map (kbd "c") 'eriks/git-timemachine-show-commit)

;;;; vdiff
(require 'vdiff)

(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(setq vdiff-auto-refine t)

(defun vdiff-close-everything (buf1 buf2)
  "close everything on exit"
  (delete-frame))

;;; mode hooks
;;;; prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; (evil-set-initial-state major-mode 'normal)
            (show-smartparens-mode t)
            (smartparens-mode t)
            (setq show-trailing-whitespace t)
            (rainbow-delimiters-mode t)
            (add-todo-font-lock)))

;;;; common c
(add-hook 'c-mode-common-hook
            (lambda ()
              (run-hooks 'abbrev-mode-hook) ;;för att den inte verkar göra det själv
              (setq-local comment-start "//")
              (setq-local comment-end "")))

;;;; c
(add-hook 'c-mode-hook
          (lambda ()
            (flycheck-mode 1)))

;;;; lisp
(defun lisp-modes-hook ()
  (eldoc-mode 1))

(dolist (l-mode '(emacs-lisp-mode-hook
                  lisp-mode-hook))
  (add-hook l-mode 'lisp-modes-hook))

;;;; html
(defun close-tag-stay ()
  (interactive)
  (save-excursion
    (sgml-close-tag)))

(add-hook 'html-mode-hook (lambda ()
                            (define-key html-mode-map (kbd "C-c C-e") 'close-tag-stay)
                            (define-key html-mode-map (kbd "/") nil)))

;;;; org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;;; python
(defun python-mode-hook-fun ()
  (setq evil-shift-width python-indent-offset)
  (flycheck-mode 1)
  (highlight-indent-guides-mode 1))

(add-hook 'python-mode-hook 'python-mode-hook-fun)

;;;; shell-script
(remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)
(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode 1)))
;;; hydras
(defhydra hydra-ggtags (:color blue :hint nil)
  "
             ggtags
-^find^----------^tag files^----^other^-----------

 [_d_]efinition  [_u_]pdate     [_Q_]uery replace
 [_r_]eference   [_c_]create    [_p_]revious mark
 [_F_]ile        [_D_]elete     [_n_]ext mark
 dwi[_m_]                     ^^[_e_]xplain
 [_s_]ymbol
 [_R_]egexp                   ^^[_q_]uit
 [_S_]how definition
 [_g_]rep
"
  ("d" ggtags-find-definition nil)
  ("S" ggtags-show-definition nil)
  ("R" ggtags-find-tag-regexp nil)
  ("Q" ggtags-query-replace nil)
  ("s" ggtags-find-other-symbol nil)
  ("r" ggtags-find-reference nil)
  ("F" ggtags-find-file nil)
  ("m" ggtags-find-tag-dwim nil)
  ("c" ggtags-create-tags nil :color red)
  ("u" ggtags-update-tags nil :color red)
  ("D" ggtags-delete-tags nil)
  ("e" ggtags-explain-tags nil)
  ("p" ggtags-prev-mark nil :color red)
  ("n" ggtags-next-mark nil :color red)
  ("g" ggtags-grep nil)
  ("q" nil nil :color blue))

(define-key eriks-map (kbd "t") 'hydra-ggtags/body)

;;; diminish
(add-hook 'autopair-mode-hook
          (lambda ()
            (diminish 'autopair-mode)))

(diminish 'counsel-mode)
(diminish 'which-key-mode)
(diminish 'ivy-mode)
(diminish 'undo-tree-mode)

(diminish 'company-mode)
(diminish 'yas-minor-mode)

(add-hook 'auto-revert-mode-hook
          (lambda ()
            (diminish 'auto-revert-mode)))

(diminish 'my-keys-minor-mode " mk")

(diminish 'outline-minor-mode (propertize " O" 'face '(:foreground "green")))

(diminish 'smartparens-mode)

(diminish 'projectile-mode)

;;doesnt run :(
(add-hook 'abbrev-mode-hook
          (lambda ()
            (diminish 'abbrev-mode)))

(diminish 'eldoc-mode)

