
;;; fonts

;; GNU unifont as fallback font
(set-fontset-font "fontset-default" nil (font-spec :size 20 :name "DejaVu Sans Mono"))

;; tilde fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)

(defface eriks-fix-later-face
  '((t :foreground "orange1"
       :background "gray10"
       :weight bold
       ))
  "Face to highlight FIXME and TODO")

(defface eriks-note-face
  '((t :foreground "chartreuse1"
       :background "gray10"
       :weight bold
       ))
  "Face to highlight NOTE")

(defun add-todo-font-lock ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\):" 1 'eriks-fix-later-face t)
                            ("\\<\\(TODO\\):" 1 'eriks-fix-later-face t)
                            ("\\<\\(NOTE\\):" 1 'eriks-note-face t))))

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

(mapc
 (lambda (m) (add-hook m 'show-paren-alt-hook))
 '(html-erb-mode-hook
   jinja2-mode-hook
   web-mode-hook
   nxml-mode-hook
   nxhtml-mode-hook
   rhtml-mode-hook
   sgml-mode-hook
   html-mode-hook
   mhtml-mode-hook
   rjsx-mode-hook))

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

;; (define-key my-keys-map (kbd "C-+") 'er/expand-region)
;; (define-key my-keys-map (kbd "M-s q") 'isearch-query-replace-regexp)
;; (define-key my-keys-map (kbd "M-s r") 'isearch-forward-regexp)
;; (define-key my-keys-map (kbd "M-s R") 'isearch-backward-regexp)
;; (define-key my-keys-map (kbd "M-s s") 'isearch-forward)
;; (define-key my-keys-map (kbd "M-s S") 'isearch-backward)

(setq line-move-visual nil)
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open")

(defun noop () "Does pretty much (exactly) nothing" (interactive))

(recentf-mode)
(setq recentf-max-menu-items 500)
(define-key my-keys-map (kbd "C-x C-r") 'counsel-recentf)
(defun recentf-save-list-silent ()
  (let ((inhibit-message t))
    (recentf-save-list)))
(run-at-time nil (* 5 60) 'recentf-save-list-silent)
(add-to-list 'recentf-exclude "recentf")

(defun describe-keymap (map)
  "Nicely shows all bindings in map"
  (interactive "sMap: ")
  (when map
    (with-output-to-temp-buffer "*describe keymap*"
      (princ (substitute-command-keys (format "\\{%s}" map))))))

(defun delete-trailing-lines ()
  (interactive)
  (and
   ;; Really the end of buffer.
   (= (goto-char (point-max)) (1+ (buffer-size)))
   (<= (skip-chars-backward "\n") -2)
   (region-modifiable-p (1+ (point)) (point-max))
   (delete-region (1+ (point)) (point-max))))

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
   ))

(defun eriks-skip-space (ARG &optional SAMELINE)
  "Move point forward or backward until it doesnt encounter whitespace anymore.
if SAMELINE then don't move the cursor between lines."
  (interactive "p")
  (let ((str (if SAMELINE " \t" " \t\n")))
    (if (< ARG 0)
        (skip-chars-backward str)
      (skip-chars-forward str)))
  ;; (if (< ARG 0)
  ;;     (while (is-whitespace (preceding-char) SAMELINE)
  ;;       (left-char))
  ;;   (while (is-whitespace (following-char) SAMELINE)
  ;;     (right-char)))
  )

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
(define-key eriks-map (kbd "gw") 'what-the-commit-insert)

(define-key eriks-map (kbd "U") 'counsel-unicode-char)

(define-key eriks-map (kbd "w U") 'untabify)
(define-key eriks-map (kbd "w u") 'tabify)
(define-key eriks-map (kbd "w w") 'whitespace-cleanup)
(define-key eriks-map (kbd "w e") 'delete-trailing-whitespace)

(define-key eriks-map (kbd "T") 'toggle-truncate-lines)
(define-key eriks-map (kbd "W") 'toggle-word-wrap)

(define-key eriks-map (kbd "f") 'counsel-file-jump)

(define-key eriks-map (kbd "k") 'describe-keymap)

(define-key eriks-map (kbd "u") 'browse-url-at-point)

(define-key eriks-map (kbd "r") 'revert-buffer)

(define-key eriks-map (kbd "m") 'man)

(define-key eriks-map (kbd "d") 'deadgrep)

(defun eriks/toggle-show-trailing-whitespace (&optional ARG)
  "Toggle `show-trailing-whitespace'. If ARG, then set to t instead of toggling."
  (interactive "P")
  (setq show-trailing-whitespace
        (if ARG
            t
          (null show-trailing-whitespace)))
  (if (interactive-p)
      (message "show trailing whitespace is %s" (if show-trailing-whitespace "on" "off"))))

(define-key eriks-map (kbd "w t") 'eriks/toggle-show-trailing-whitespace)

(defun eriks/clone-indirect-buffer-other-window (NEWNAME DISPLAY-FLAG &optional NORECORD)
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
	 (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
	       (read-buffer "Name of indirect buffer: " (current-buffer))
             (concat (buffer-name) "[clone]"))
	   t)))
  (clone-indirect-buffer-other-window NEWNAME DISPLAY-FLAG NORECORD))

(define-key eriks-map (kbd "c") 'eriks/clone-indirect-buffer-other-window)

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
(require 'avy)
(require 'hide-lines)
(require 'highlight-indent-guides)

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

(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(counsel-projectile-mode)

(defun ggtags-mode-hook-fun ()
  (setq ggtags-mode-line-project-name nil))
(add-hook 'ggtags-mode-hook 'ggtags-mode-hook-fun)

;; disable linum-mode in main config file (this one)
;; (add-hook 'outline-minor-mode-hook (lambda ()
;;                                      (when (and
;;                                             buffer-file-name
;;                                             (equal "myinit.el"
;;                                                     (replace-regexp-in-string "\\(^.*/\\).*?$" "" buffer-file-name nil nil 1)))
;;                                        (linum-mode -1))))

(define-key evil-normal-state-map (kbd "SPC h l") 'hide-lines-not-matching)
(define-key evil-normal-state-map (kbd "SPC h L") 'hide-lines-matching)
(define-key evil-normal-state-map (kbd "SPC h s") 'hide-lines-show-all)

;;;; ivy, swiper and counsel
(ivy-mode 1)
(counsel-mode 1)
(define-key counsel-mode-map [remap describe-bindings] nil)
(define-key my-keys-map (kbd "C-s") 'swiper)
(define-key my-keys-map (kbd "M-s") 'counsel-rg)
(define-key ivy-minibuffer-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-u") 'ivy-kill-line)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-b") 'ivy-scroll-down-command)
(define-key ivy-minibuffer-map (kbd "C-f") 'ivy-scroll-up-command)
(setq ivy-use-selectable-prompt t)

;;;; avy
(setq avy-keys '(;;nconc
                ?h ?g ?j ?f ?k ?d ?l ?s
                ;; (number-sequence ?a ?z)
                ;; '(?å ?ä ?ö)
                ))

;; no timeout ("infinite" timeout)
(setq avy-timeout-seconds nil)
(add-to-list 'avy-orders-alist '(avy-goto-char-timer . avy-order-closest))

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

(defun eriks/avy-goto-char-timer-advice (f &rest rest)
  "Advice to rebind SPC to RET."
  ;; why doesn't both old-read-char and read-char refer to the new temporary lambda?
  (cl-letf* ((old-read-char (symbol-function #'read-char))
             ((symbol-function #'read-char) (lambda (&rest re)
                                             (let ((key (apply old-read-char re)))
                                               (if (= key 32)
                                                   13
                                                 key)))))
    (apply f rest)))
(advice-add 'avy-goto-char-timer :around #'eriks/avy-goto-char-timer-advice)

;;;; smartparens
;; remove defaults
;(sp-pair "\\\\(" nil :actions :rem)
;(sp-pair "\\{"   nil :actions :rem)
;(sp-pair "\\("   nil :actions :rem)
;(sp-pair "\\\""  nil :actions :rem)

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair '(c-mode java-mode css-mode js-mode rust-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair '(js-mode) "[" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;;;; outshine
;; (setq outshine-use-speed-commands t)
(defun outline-minor-mode-hook-fun ()
  (outshine-mode)
  (define-key outline-minor-mode-map [remap self-insert-command] nil) ;;remove annyoing remap to outshine-self-insert-command
  (define-key outshine-mode-map [remap self-insert-command] nil)
  )
(add-hook 'outline-minor-mode-hook 'outline-minor-mode-hook-fun)

;; removed top-level sexpressions as outlines in lisp modes
(defun lisp-outline-hook-fun ()
  (setq outline-regexp ";;;\\(;* [^ 	
]\\|###autoload\\)"))

(dolist (l-mode '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook l-mode 'lisp-outline-hook-fun))

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
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*dashboard\\*$")
                         (mode . help-mode)
                         (name . "^\\*Customize.*")))
               ;; ("magit" (name . "^\\*magit:.*"))
               ))))

;; (defun ibuffer-mode-hook-fun ()
;;   (ibuffer-switch-to-saved-filter-groups "default"))
;; (add-hook 'ibuffer-mode-hook 'ibuffer-mode-hook-fun)

(define-key my-keys-map (kbd "C-x C-b") 'ibuffer)

;;;; MATLAB

(require 'matlab)

(defun matlab-mode-hook-fun ()
  (run-hooks 'prog-mode-hook))
(add-hook 'matlab-mode-hook 'matlab-mode-hook-fun)

;;;; latex

(require 'latex)
(require 'auctex-latexmk)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(auctex-latexmk-setup)

;; default viewer
(setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")

(define-key LaTeX-mode-map (kbd "C-c e") 'TeX-error-overview)

(defvar eriks/latex-autocompile nil "Whether the current buffer's after-save-hook should compile the document")
(make-variable-buffer-local 'eriks/latex-autocompile)
(defvar eriks/latex-autocompile-command nil "TeX command to auto compile with")
(make-variable-buffer-local 'eriks/latex-autocompile-command)

(defun eriks/latex-autocompile-toggle ()
  "Toggles whether the document should be compiled after each save or not.
Queries the command to use when toggling on."
  (interactive)
  (setq eriks/latex-autocompile-command nil)
  (when (not eriks/latex-autocompile)
    (setq eriks/latex-autocompile-command (TeX-command-query (TeX-master-file nil nil t))))
  (setq eriks/latex-autocompile (and (not eriks/latex-autocompile)
                                     eriks/latex-autocompile-command))
  (message "auto compilation is %s" (if eriks/latex-autocompile "on" "off")))

(define-key LaTeX-mode-map (kbd "C-c a") 'eriks/latex-autocompile-toggle)

(defun eriks/latex-compile-on-save ()
  "Auto compile on save if enabled.
see `eriks/latex-autocompile-toggle'"
  (when (and eriks/latex-autocompile
             eriks/latex-autocompile-command)
    (TeX-command eriks/latex-autocompile-command 'TeX-active-master)))

(defun LaTeX-mode-hook-fun ()
  ;; (modify-syntax-entry ?$ "\"" LaTeX-mode-syntax-table) ;;make $ act like string so smartparens can navigate with it.
  (add-hook 'after-save-hook 'eriks/latex-compile-on-save nil t)
  (dolist (i '(paragraph-start paragraph-separate))
    (set i (default-value i)))
  (TeX-source-correlate-mode 1)
  (run-hooks 'prog-mode-hook))
(add-hook 'LaTeX-mode-hook 'LaTeX-mode-hook-fun)

;;;; company
(global-company-mode t)
(define-key my-keys-map (kbd "C-SPC") 'company-complete)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)

;;;; windows

(require 'ace-window)
(define-key my-keys-map (kbd "C-x o") 'ace-window)

(frames-only-mode)
(setq frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops nil)

(define-key my-keys-map (kbd "C-x C-0") 'kill-buffer-and-frame)

(defun kill-buffer-and-frame ()
  "Kills the current buffer, if successful then delete the frame."
  (interactive)
  (when (and
         (buffer-modified-p)
         (y-or-n-p "Current buffer is modified, save?"))
    (save-buffer))
  (when (kill-buffer)
    (delete-frame)))

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
(define-key evil-insert-state-map (kbd "S-SPC") (lambda () (interactive) (insert ?\s)))

(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

(define-key evil-insert-state-map (kbd "<up>") 'noop)
(define-key evil-insert-state-map (kbd "<left>") 'noop)
(define-key evil-insert-state-map (kbd "<right>") 'noop)
(define-key evil-insert-state-map (kbd "<down>") 'noop)

;; fix https://github.com/company-mode/company-mode/issues/15
(eval-after-load 'company
  '(evil-declare-change-repeat 'company-complete))

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
(evil-set-initial-state 'org-mode 'normal)
(evil-set-initial-state 'Man-mode 'motion)
(evil-set-initial-state 'conf-mode 'normal)

(define-key evil-normal-state-map (kbd "SPC :") 'eval-expression)
(define-key evil-normal-state-map (kbd "SPC ;") 'set-variable)
(define-key evil-normal-state-map (kbd "SPC x") 'eriks/calc-eval-region)
(define-key evil-normal-state-map (kbd "SPC X") 'eriks/calc-convert-bases)

(define-key evil-motion-state-map (kbd "C-d") 'golden-ratio-scroll-screen-up)
(define-key evil-motion-state-map (kbd "C-u") 'golden-ratio-scroll-screen-down)
(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

(defun eriks/calc-eval-region (arg beg end)
  "runs `calc-eval' on the active region, preserves newlines.
Only works on one line at a time.

Set the output base with `calc-radix'.
With any prefix argument, remove the base indicator in the output (16#FF -> FF)"
  (interactive "P\nr")
  (when (region-active-p)
    (let* ((s (delete-and-extract-region beg end))
           (has-newline (equal (substring s -1 nil) "\n"))
           (shell-res (calc-eval s)))
      (when arg
        (setq shell-res (replace-regexp-in-string "^[[:digit:]]+#" "" shell-res)))
      (insert shell-res)
      (when has-newline
        (insert "\n")))))

(defun eriks/calc-convert-bases (ibase obase beg end)
  "Converts a number in selection to another with the prompted bases."
  (interactive
   (if (region-active-p)
       (list (read-number "input base: " 10)
             (read-number "output base: " 10)
             (region-beginning) (region-end))
     (message "no selection active")
     (list nil nil nil nil)))
  (when (and ibase obase beg end)
    (let* ((s (delete-and-extract-region beg end))
           (has-newline (equal (substring s -1 nil) "\n"))
           shell-res
           (inhibit-message t)
           (old-radix calc-number-radix))
      (calc-radix obase)
      (setq shell-res (replace-regexp-in-string "^[[:digit:]]+#" "" (calc-eval (format "%s#%s" ibase s))))
      (insert shell-res)
      (when has-newline
        (insert "\n"))
      (calc-radix old-radix))))

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
(define-key evil-motion-state-map (kbd "<backspace>") 'evil-ex-nohighlight)

;; (evil-remap "C-@" "@@")

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
(define-key evil-normal-state-map (kbd "M-c") 'evil-capitalize-last-word)

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
open-line doesn't indent the new line in any way)

If the line to be split is a comment, run `comment-indent-new-line'
instead (splits, adds comment chars and indents)."
  (interactive "P")
  (let ((start-ind (current-indentation))
        (raw (equal ARG '(4)))
        (in-comment (nth 4 (syntax-ppss)))
        method)
    (just-one-space 0)
    (if in-comment
        (save-excursion
          (comment-indent-new-line))
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
                   (indent-to start-ind)))))))))

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

;;opposite to J (join-lines)
(define-key evil-normal-state-map (kbd "S") 'evil-open-line)
(define-key evil-normal-state-map (kbd "<return>")   'evil-open-line-below)
(define-key evil-normal-state-map (kbd "S-<return>") 'evil-open-line-above)

(defun eriks/evil-open-below ()
  "pretty much the same as `evil-open-below' except that this
continues a comment if we are in one"
  (interactive)
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (push (point) buffer-undo-list)
  (end-of-line)
  (comment-indent-new-line)
  (evil-insert-state 1))

(define-key evil-normal-state-map (kbd "SPC o") 'eriks/evil-open-below)

;; copy of the normal evil-join
(defmacro eriks/evil-join-template (name doc &rest BODY)
  "Creates an evil operator named 'eriks/evil-join-{name}' that runs
BODY after each time a line is joined."
  `(evil-define-operator ,(intern (concat "eriks/evil-join-" (symbol-name name))) (beg end)
     ,(concat "Join the selected lines just as `evil-join', but with a twist!\n" doc)
     :motion evil-line
     (let ((count (count-lines beg end)))
       (when (> count 1)
         (setq count (1- count)))
       (goto-char beg)
       (dotimes (var count)
         (join-line 1)
         ,@BODY))))

(eriks/evil-join-template no-space
 "This one with no space inbetween"
 (just-one-space 0))

(eriks/evil-join-template no-comment
 "This one removes comments defined in `comment-start-skip'"
 (when (and (nth 4 (syntax-ppss)) ; if previous line is a comment
            (looking-at (concat "\\( *\\)" comment-start-skip)))
   (replace-match "\\1")
   (goto-char (match-beginning 0))
   (just-one-space)))

(define-key evil-normal-state-map (kbd "SPC J") 'eriks/evil-join-no-space)
(define-key evil-visual-state-map (kbd "SPC J") 'eriks/evil-join-no-space)
(define-key evil-normal-state-map (kbd "J") 'eriks/evil-join-no-comment)
(define-key evil-visual-state-map (kbd "J") 'eriks/evil-join-no-comment)

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

;;;;; macro
(defun eriks/evil-better-record-macro (_)
  "When done recording a macro, set `evil-last-register' to the newly
recorded register so that `evil-execute-macro' will believe that this
macro was the last executed one."
  (when (and evil-this-macro defining-kbd-macro)
    (setq evil-last-register evil-this-macro)))
(advice-add 'evil-record-macro :before #'eriks/evil-better-record-macro)

(define-key evil-normal-state-map (kbd "SPC q") 'kmacro-insert-counter)
(define-key evil-normal-state-map (kbd "SPC Q") 'kmacro-set-counter)

;;;;; evil-args

(require 'evil-args)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

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

(define-key evil-normal-state-map (kbd "SPC s j") 'eriks-evil-javadoc-search-operator)
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
(define-key evil-visual-state-map (kbd "C-s") 'eriks-evil-swiper-operator)

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

(define-key evil-normal-state-map (kbd "SPC s g") 'grep-find)

(define-key evil-normal-state-map (kbd "SPC s f") 'find-dired)

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
(require 'evil-collection-help)
(defun evil-collection-help-setup ()
  "Set up `evil' bindings for `help'."
  (evil-set-initial-state 'help-mode 'normal)
  (evil-collection-inhibit-insert-state 'help-mode-map)
  (evil-collection-define-key 'normal 'help-mode-map
    ;; motion
    (kbd "C-f") 'scroll-up-command
    (kbd "C-b") 'scroll-down-command
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    (kbd "C-o") 'help-go-back
    (kbd "C-i") 'help-go-forward

    "go" 'push-button
    "gO" 'push-button
    (kbd "<return>") 'push-button

    "g?" 'describe-mode
    "gr" 'revert-buffer
    "<" 'help-go-back
    ">" 'help-go-forward
    "r" 'help-follow

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))
(evil-collection-help-setup)

;;;;;; outline minor mode
(evil-define-key '(normal visual motion) outline-minor-mode-map
  (kbd "zo")  'outline-show-more
  (kbd "zp")  'outline-hide-other
  (kbd "zB")  'outline-hide-body
  (kbd "zb")  'outline-hide-entry
  (kbd "ze")  'outline-show-entry
  ;; (kbd "zl")  'outline-hide-leaves ;; interferes with evil binding
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
(define-key evil-normal-state-map (kbd ";") 'repeat)

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
  (avy-with avy-goto-char
    (avy--process
     (eriks/avy--regex-candidates-exclusive
      (regexp-quote (string char))
      (line-beginning-position)
      (line-end-position))
     (avy--style-fn avy-style))))

(defun eriks/avy-order-closest-single (x)
  "`avy-order-closest' doesn't like x on the form (p1 . w), but likes ((p1 . p2) . w)"
  (avy-order-closest `((,(car x) . ,(car x)) . ,(cdr x))))

(defun eriks/avy-goto-line-first-non-blank ()
  (interactive)
  (avy-with eriks/avy-goto-line-first-non-blank
    (let ((points (nconc (evilem--collect #'evil-next-line-first-non-blank)
                         (evilem--collect #'evil-previous-line-first-non-blank))))
      (avy--process points (avy--style-fn avy-style)))))

(add-to-list 'avy-orders-alist '(eriks/avy-goto-line-first-non-blank . eriks/avy-order-closest-single))
(evil-define-avy-motion eriks/avy-goto-line-first-non-blank line)

(evil-define-avy-motion eriks/avy-goto-char-in-line-exclusive inclusive)
(define-key evil-motion-state-map (kbd ".") 'evil-eriks/avy-goto-char-in-line-exclusive)

(define-key evil-motion-state-map (kbd ",") 'avy-goto-char-in-line)
(define-key evil-motion-state-map (kbd "SPC ,") 'avy-goto-char)

;; (define-key evil-motion-state-map (kbd ";") 'avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "SPC SPC") 'avy-goto-char-timer)

(define-key evil-normal-state-map (kbd "-") 'negative-argument)
(define-key evil-motion-state-map (kbd "+") 'evil-eriks/avy-goto-line-first-non-blank)
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

(evilem-define (kbd "SPC ]s") 'forward-sentence)
(evilem-define (kbd "SPC [s") 'backward-sentence)

(evilem-define (kbd "SPC [i") 'eriks-up-indentation)

(evilem-define (kbd "SPC [u") 'sp-backward-up-sexp)
(evilem-define (kbd "SPC ]u") 'sp-up-sexp)
(evilem-define (kbd "SPC [d") 'sp-backward-down-sexp)
(evilem-define (kbd "SPC ]d") 'sp-down-sexp)

;;;;; motion map
(define-key evil-motion-state-map (kbd "[m") 'evil-backward-section-begin)
(define-key evil-motion-state-map (kbd "[M") 'evil-backward-section-end)
(define-key evil-motion-state-map (kbd "]m") 'evil-forward-section-begin)
(define-key evil-motion-state-map (kbd "]M") 'evil-forward-section-end)

(define-key evil-motion-state-map (kbd "]s") 'forward-sentence)
(define-key evil-motion-state-map (kbd "[s") 'backward-sentence)

(define-key evil-motion-state-map (kbd "][") 'sp-next-sexp)
(define-key evil-motion-state-map (kbd "]]") 'sp-forward-sexp) ;; sp-forward-parallel-sexp
(define-key evil-motion-state-map (kbd "[[") 'sp-backward-sexp)
(define-key evil-motion-state-map (kbd "[]") 'sp-previous-sexp)

(define-key evil-motion-state-map (kbd "[f") 'beginning-of-defun)
(define-key evil-motion-state-map (kbd "]f") 'end-of-defun)

(define-key evil-motion-state-map (kbd "[i") 'eriks-up-indentation)

(define-key evil-motion-state-map (kbd "[c") 'eriks-up-same-column)
(define-key evil-motion-state-map (kbd "]c") 'eriks-down-same-column)

(define-key evil-motion-state-map (kbd "[ SPC") 'eriks-skip-space-backwards)
(define-key evil-motion-state-map (kbd "] SPC") 'eriks-skip-space)

(define-key evil-motion-state-map (kbd "[u") 'sp-backward-up-sexp)
(define-key evil-motion-state-map (kbd "]u") 'sp-up-sexp)
(define-key evil-motion-state-map (kbd "[d") 'sp-backward-down-sexp)
(define-key evil-motion-state-map (kbd "]d") 'sp-down-sexp)
(define-key evil-motion-state-map (kbd "(") 'sp-beginning-of-sexp)
(define-key evil-motion-state-map (kbd ")") 'sp-end-of-sexp)

;;;;; evil-exchange
(require 'evil-exchange)
(evil-exchange-install)

;;;;; evil-multiedit
(require 'evil-multiedit)

(setq evil-multiedit-follow-matches t)

(define-key iedit-mode-occurrence-keymap (kbd "M-n") nil)
(define-key iedit-mode-occurrence-keymap (kbd "M-p") nil)

;; Highlights all matches of the selection in the buffer.
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; incrementally add the next unmatched match.
(define-key evil-normal-state-map (kbd "C-n") 'evil-multiedit-match-and-next)
;; Match selected region.
(define-key evil-visual-state-map (kbd "C-n") 'evil-multiedit-match-and-next)
;; Insert marker at point
(define-key evil-insert-state-map (kbd "C-n") 'evil-multiedit-toggle-marker-here)

;; Same as M-d but in reverse.
(define-key evil-normal-state-map (kbd "C-p") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "C-p") 'evil-multiedit-match-and-prev)

;; OPTIONAL: If you prefer to grab symbols rather than words, use
;; `evil-multiedit-match-symbol-and-next` (or prev).

;; Restore the last group of multiedit regions.
(define-key evil-normal-state-map (kbd "gm") 'evil-multiedit-restore)

;; RET will toggle the region under the cursor
(define-key evil-multiedit-state-map (kbd "<return>") 'evil-multiedit-toggle-or-restrict-region)

;; ...and in visual mode, RET will disable all fields outside the selected region
(define-key evil-motion-state-map (kbd "<return>") 'evil-multiedit-toggle-or-restrict-region)

;; For moving between edit regions
(define-key evil-multiedit-state-map (kbd "M-n") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "M-p") 'evil-multiedit-prev)
(define-key evil-multiedit-insert-state-map (kbd "M-n") 'evil-multiedit-next)
(define-key evil-multiedit-insert-state-map (kbd "M-p") 'evil-multiedit-prev)

;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

;;;;; ggtags

;; (define-key evil-normal-state-map (kbd "SPC SPC") 'ggtags-find-tag-dwim)
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

(defun latex-surround-env ()
  (let ((ename (or (read-from-minibuffer "environment: ") "")))
    (cons (format "\\begin{%s}" ename) (format "\\end{%s}" ename))))

(defun LaTeX-mode-hook-textobj-fun ()
  ;; (add-to-list 'evil-surround-pairs-alist '(?f . erik-evil-surround-latex-macro))
  (add-to-list 'evil-surround-pairs-alist '(?$ . ("$" . "$")))
  (add-to-list 'evil-surround-pairs-alist '(?m . latex-surround-env))
  (evil-define-inner-local-textobject "$" 'evil-latex-textobjects-inner-dollar)
  (evil-define-outer-local-textobject "$" 'evil-latex-textobjects-a-dollar)
  (evil-define-inner-local-textobject "\\" 'evil-latex-textobjects-inner-math)
  (evil-define-outer-local-textobject "\\" 'evil-latex-textobjects-a-math)
  (evil-define-inner-local-textobject "f" 'evil-latex-textobjects-inner-macro)
  (evil-define-outer-local-textobject "f" 'evil-latex-textobjects-a-macro)
  (evil-define-inner-local-textobject "m" 'evil-latex-textobjects-inner-env)
  (evil-define-outer-local-textobject "m" 'evil-latex-textobjects-an-env))
(add-hook 'LaTeX-mode-hook 'LaTeX-mode-hook-textobj-fun)

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

(define-key eriks-map (kbd "gg") 'git-gutter+-mode)
(define-key eriks-map (kbd "gj") 'git-gutter+-next-hunk)
(define-key eriks-map (kbd "gk") 'git-gutter+-previous-hunk)
(define-key eriks-map (kbd "gr") 'git-gutter+-revert-hunk)

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
(define-key git-timemachine-mode-map (kbd "c") 'git-timemachine-show-commit)

;;;; org-mode
(defun eriks/org-mode-hook-fun ()
  (toggle-truncate-lines 0)
  (toggle-word-wrap 1))
(add-hook 'org-mode-hook 'eriks/org-mode-hook-fun)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(textobjects insert navigation additional todo))
(evil-define-key 'normal evil-org-mode-map
  (kbd "go") (evil-org-define-eol-command org-insert-heading-respect-content)
  (kbd "gO") (evil-org-define-eol-command org-insert-subheading)
  (kbd "T")  (evil-org-define-eol-command org-insert-todo-heading-respect-content)
  (kbd "gt") (evil-org-define-eol-command org-insert-todo-subheading)
  (kbd "H-j") 'org-forward-element
  (kbd "H-k") 'org-backward-element
  (kbd "H-h") 'org-up-element
  (kbd "H-l") 'org-down-element
  (kbd "H-u") 'evil-org-top)

;; (require 'evil-org-agenda)
;; (evil-org-agenda-set-keys)
(setq org-agenda-files '("~/Dropbox/org"))
(define-key eriks-map (kbd "a") 'org-agenda)

(defun find-my-org-agenda-files ()
  "Visit any org file in `org-agenda-files'"
  (interactive)
  (find-file
   (ivy-read "Org Agenda file: "
             (org-agenda-files)
             :require-match t)))
(define-key eriks-map (kbd "o") 'find-my-org-agenda-files)

(define-key org-mode-map (kbd "C-c s") 'counsel-org-goto-all)

;;;;; org bullets and pretty stuff
(require 'org-bullets)

(setq org-bullets-bullet-list '("▶" "▼" "◀" "▲"))

(defun org-mode-bullets-hook-fun ()
  (org-bullets-mode 1))
(add-hook 'org-mode-hook 'org-mode-bullets-hook-fun)

(setq org-ellipsis "↴")
(set-face-attribute 'org-ellipsis nil :foreground "burlywood" :height 0.9)

;;;;; org auto format
(add-to-list 'safe-local-variable-values '(eval eriks/org-format))

(defun eriks/org-format ()
  "Tries to neatly format an Org mode file.
What it tries to do:
  - make sure that there is exactly one empty line before any top level heading
  - deletes trailing lines"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (cl-labels ((donext ()
                        (let ((p (re-search-forward "^\\* " nil t)))
                          (when p
                            (save-excursion
                              (forward-line -1)
                              (just-one-line))
                            (donext))))
                (just-one-line ()
                               (end-of-line)
                               (let ((cur (point)))
                                 (skip-chars-backward " \t\n")
                                 (delete-region cur (point)))
                               (newline)))
      (donext)
      (delete-trailing-lines))))

;;;;; evil-org-mode bug
(defun eriks/evil-org-select-an-element-fix (_f element)
  "Used `region-beginning' before without checking if `region-active-p' or similar."
  (list (min
         (if (region-active-p) (region-beginning) (point))
         (org-element-property :begin element))
        (org-element-property :end element)))
(advice-add 'evil-org-select-an-element :around #'eriks/evil-org-select-an-element-fix)

;;;;; remove done tasks
(defun erik/org-remove-done-tasks ()
  "delete all headers marked as DONE on the current subtree"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-mark-subtree)
     (delete-region (region-beginning) (region-end))
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

;;; mode hooks
;;;; prog-mode
(defun prog-mode-hook-fun ()
  ;; (evil-set-initial-state major-mode 'normal)
  (show-smartparens-mode t)
  (smartparens-mode t)
  (eriks/toggle-show-trailing-whitespace t)
  (rainbow-delimiters-mode t)
  (add-todo-font-lock))
(add-hook 'prog-mode-hook 'prog-mode-hook-fun)

;;;; common c
(defun c-mode-common-hook-fun ()
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (abbrev-mode -1))
(add-hook 'c-mode-common-hook 'c-mode-common-hook-fun)

;;;; c
(defun c-mode-hook-fun ()
  (flycheck-mode 1))
(add-hook 'c-mode-hook 'c-mode-hook-fun)

;;;; lisp
(defun lisp-modes-hook ()
  (eldoc-mode 1))

(dolist (l-mode '(emacs-lisp-mode-hook
                  lisp-mode-hook))
  (add-hook l-mode 'lisp-modes-hook))

;;;; html
(put 'sgml-basic-offset 'safe-local-variable 'integerp)

(defun close-tag-stay ()
  (interactive)
  (save-excursion
    (sgml-close-tag)))

(defun html-mode-hook-fun ()
  (define-key html-mode-map (kbd "C-c C-e") 'close-tag-stay)
  (define-key html-mode-map (kbd "/") nil))
(add-hook 'html-mode-hook 'html-mode-hook-fun)

;;;; python
(defun python-mode-hook-fun ()
  (setq evil-shift-width python-indent-offset)
  (flycheck-mode 1)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (highlight-indent-guides-mode 1))

(add-hook 'python-mode-hook 'python-mode-hook-fun)

;;;; shell-script
(remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)
(defun sh-mode-hook-fun ()
  (flycheck-mode 1))
(add-hook 'sh-mode-hook 'sh-mode-hook-fun)

;;;; rust
(defun rust-mode-hook-fun ()
  (flycheck-mode 1)
  (flycheck-rust-setup))
(add-hook 'rust-mode-hook 'rust-mode-hook-fun)

;;;; haskell
(put 'haskell-indentation-left-offset 'safe-local-variable 'integerp)

(defun haskell-mode-hook-fun ()
  ;; (highlight-indent-guides-mode 1)
  (flycheck-mode 1)
  (flycheck-select-checker 'haskell-hlint)
  (haskell-doc-mode 1)
  )
(add-hook 'haskell-mode-hook 'haskell-mode-hook-fun)

;;;; diff
(defun diff-mode-hook-fun ()
  (diff-auto-refine-mode -1))
(add-hook 'diff-mode-hook 'diff-mode-hook-fun)

;;;; man-mode
(require 'man)
(define-key Man-mode-map (kbd "q") 'kill-buffer-and-frame)
(defun man-mode-hook-fun ()
  (face-remap-set-base 'default '(:foreground "#f8f8f2")))
(add-hook 'Man-mode-hook 'man-mode-hook-fun)

;;;; javascript-mode
;; (add-hook 'js-mode-hook 'js2-minor-mode)

;;;; rjsx-mode
(sp-local-pair '(js-jsx-mode js2-jsx-mode rjsx-mode) "<" nil :actions :rem)
(defun rjsx-mode-hook-fun ()
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))
(add-hook 'rjsx-mode-hook #'rjsx-mode-hook-fun)

;;;; m4-mode
(sp-local-pair 'm4-mode "`" "'" :actions '(insert autoskip navigate))

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

(require 'diminish)
(defmacro diminish-after (FILE MODE &optional TO-WHAT)
  "runs `diminish' with MODE and TO-WHAT after FILE has loaded"
  (let ((f FILE))
    `(if (featurep ,f)
         (eval-after-load ,f (quote (diminish ,MODE ,TO-WHAT)))
       (warn "featurep not satisfied for %s" ,f))))

(diminish-after 'counsel 'counsel-mode)
(diminish-after 'which-key 'which-key-mode)
(diminish-after 'ivy 'ivy-mode)
(diminish-after 'undo-tree 'undo-tree-mode)

(diminish-after 'company 'company-mode)
(diminish-after 'yasnippet 'yas-minor-mode)

(diminish 'my-keys-minor-mode " mk")

(diminish-after 'outline 'outline-minor-mode (propertize " O" 'face '(:foreground "green")))

(diminish-after 'smartparens 'smartparens-mode)

(diminish-after 'projectile 'projectile-mode)

(diminish-after 'eldoc 'eldoc-mode " Doc")

(diminish-after 'evil-org 'evil-org-mode)

(diminish-after 'highlight-indent-guides 'highlight-indent-guides-mode)

(diminish-after 'outshine 'outshine-mode)
