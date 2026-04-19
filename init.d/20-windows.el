;;NOTE: recommended by https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq-default switch-to-buffer-obey-display-actions t)

;; Random variables
(setq-default fit-window-to-buffer-horizontally t)

;; mode-line
(progn
  (defface eriks/mode-line-error-face '((t :foreground "red" :weight bold))
    "Face for bl.a. the buffer modified asterisks in the mode line")
  (defface eriks/mode-line-warning-face '((t :foreground "yellow"))
    "Face for bl.a. the buffer read only percentages in the mode line")

  (define-advice mode-line-eol-desc (:filter-return (desc) reddify)
    "Make the mode line red if the current buffer uses non-LF line endings."
    (let ((eol (coding-system-eol-type buffer-file-coding-system)))
      (cond ((or (vectorp eol)
                 (eql eol 0))
             desc)
            (t (propertize desc 'face 'eriks/mode-line-error-face)))))

  (defun eriks/mode-line-escape (str)
    "Escape a string for the mode line by putting it behind a symbol.

All strings in `mode-line-format' that has %-constructs get them
substituted for various kinds of data, except if the string was the
value of a symbol."
    (let ((sym (make-symbol "magic")))
      (put sym 'risky-local-variable t)
      (set sym str)
      sym))

  (defun eriks/mode-line-dim (fmt)
    "Removes all text properties of the given mode line format if window is not selected."
    (eriks/mode-line-escape (format-mode-line fmt (and (not (mode-line-window-selected-p)) 1))))

  (defun eriks/mode-line-buffer-identification ()
    "Mode line thingy for buffer identification."
    (let ((path (buffer-file-name))
          (name (buffer-name)))
      (eriks/mode-line-escape
       (propertize (if path
                       (fish-path (abbreviate-file-name path) :lastfull 2 :complen 0)
                     name)
                   'face 'mode-line-buffer-id
                   ;; TODO: also add buffer-file-truename
                   'help-echo (format "File: %s\nBuffer: %s\nDefault directory: %s" path name default-directory)
                   'mouse-face 'mode-line-highlight))))

  (defun eriks/mode-line-modified ()
    "Buffer modified and read only mode line thingies."
    (let ((mod-char (propertize "*" 'face 'eriks/mode-line-error-face))
          (ro-char (propertize "%%" 'face 'eriks/mode-line-warning-face))
          (norm-char "-"))
      (list (propertize (cond (buffer-read-only ro-char)
                              ((buffer-modified-p) mod-char)
                              (t norm-char))
                        'help-echo 'mode-line-read-only-help-echo
                        'mouse-face 'mode-line-highlight)
            (propertize (cond ((buffer-modified-p) mod-char)
                              (buffer-read-only ro-char)
                              (t norm-char))
                        'help-echo 'mode-line-modified-help-echo
                        'mouse-face 'mode-line-highlight))))

  (put 'mode-line-format 'original-value (default-value 'mode-line-format))

  (setq-default mode-line-position-column-line-format '(" (%l,%C)")
                mode-line-percent-position '("%q")
                mode-line-buffer-identification '((projectile-mode ("" projectile--mode-line " "))
                                                  (:eval (eriks/mode-line-buffer-identification)))
                mode-line-modified '(:eval (eriks/mode-line-modified))
                mode-line-compact 'long
                mode-line-format '((:eval (eriks/mode-line-dim (get 'mode-line-format 'original-value)))))

  (column-number-mode 1))

(use-package popper
  :ensure t
  :config
  (eriks/add-to-list 'popper-reference-buffers
                     (eriks/regexp-quote-all "*Warnings*")
                     'evil-list-view-mode
                     (eriks/regexp-quote-all "*eshell*")
                     (eriks/regexp-quote-all "*shell*")
                     (eriks/regexp-quote-all trace-buffer)
                     'inferior-emacs-lisp-mode
                     'inferior-python-mode
                     'messages-buffer-mode
                     'flycheck-verify-mode
                     'flymake-project-diagnostics-mode
                     'flymake-diagnostics-buffer-mode
                     'process-menu-mode
                     (eriks/regexp-quote-all "*Shell Command Output*")
                     "^\\*apheleia-.+-log\\*$"
                     "^\\*eldoc"
                     "^eglot-macro-"
                     'compilation-mode
                     (eriks/regexp-quote-all "*Pp Macroexpand Output*")
                     (eriks/regexp-quote-all "*Pp Eval Output*")
                     'help-mode
                     'debugger-mode
                     'calendar-mode)

  (defun eriks/popper-display-popup-on-the-right-advice (buffer &optional alist)
    "Advice to override what `popper-display-popup-at-bottom' does."
    (display-buffer-in-side-window
     buffer
     (append alist
             `((window-width . ,popper-window-width)
               (side . right)
               (slot . 0)))))

  (defun eriks/popper-toggle-display-function ()
    "Toggle which side to use when displaying popup buffers."
    ;; TODO: make this respect `popper-display-function'
    (interactive)
    (if (advice-member-p 'eriks/popper-display-popup-on-the-right-advice 'popper-display-popup-at-bottom)
        (advice-remove 'popper-display-popup-at-bottom 'eriks/popper-display-popup-on-the-right-advice)
      (advice-add 'popper-display-popup-at-bottom :override 'eriks/popper-display-popup-on-the-right-advice))

    (cl-loop for (win . buf) in popper-open-popup-alist
             do (progn
                  (delete-window win)
                  (display-buffer buf))))

  (defun eriks/popper-no-select-advice (fun &rest args)
    "Advice the function to display using popper without selecting the window."
    ;; TODO: make this respect `popper-display-function'
    (let ((display-buffer-alist (cons '(popper-display-control-p (popper-display-popup-at-bottom))
                                      display-buffer-alist)))
      (apply fun args)))

  (popper-mode 1)
  (popper-echo-mode 1)

  (eriks/defkey-repeat (popper-cycle)
    :keymaps 'popper-mode-map
    :prefix "C-x"
    "<right>" 'popper-cycle
    "<left>" 'popper-cycle-backwards)
  :custom
  (popper-mode-line-position 1) ;; NOTE: move past ace-window
  (popper-mode-line '(:eval (eriks/mode-line-dim (propertize " POP" 'face 'mode-line-emphasis))))
  (popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (popper-window-width (cl-defun eriks/popper-fit-window-width (win)
                         "The is inspired by the default to adjust the width instead of the height."
                         (fit-window-to-buffer
                          win
                          nil
                          nil
                          (floor (frame-width) 2)
                          (floor (frame-width) 3))))
  (popper-window-height (cl-defun eriks/popper-fit-window-height (win)
                          "The same as the default, but with tweaked minimum and maximum."
                          (fit-window-to-buffer
                           win
                           (floor (frame-height) 3)
                           (floor (frame-height) 4))))
  :general-config
  ('popper-mode-map
   :prefix "C-x p"
   "s" 'eriks/popper-toggle-display-function
   "t" 'popper-toggle-type
   "k" 'popper-kill-latest-popup)
  ('popper-mode-map
   :prefix "C-x"
   "o" 'popper-toggle))

(use-package ace-window
  :ensure t
  :custom
  ;; NOTE: the global scope doesn't work https://github.com/abo-abo/ace-window/issues/247
  ;; It seems like emacs can't set the focus with my xmonad config
  ;; https://github.com/abo-abo/ace-window/issues/241
  (aw-scope 'frame)
  (aw-display-mode-overlay nil)
  (aw-dispatch-always t)
  ;; NOTE: free bindinds: an,.lir
  (aw-keys '(?f ?j ?d ?k))
  (aw-dispatch-alist '((?x aw-delete-window "Delete window")
                       (?X kill-buffer-and-window "Delete window and kill buffer")
                       (?q eriks/aw-quit-window "Quit window")
                       (?Q eriks/aw-quit-kill-window "Quit window and kill buffer")

                       ;; NOTE: z = `aw-make-frame-char'

                       (?s aw-swap-window "Swap windows")
                       (?m aw-move-window "Move window")

                       (?p toggle-window-dedicated "Toggle dedicated")

                       (?c eriks/aw-clone-buffer "Clone buffer")

                       (?g aw-switch-buffer-in-window "Switch buffer goto")
                       (?b aw-switch-buffer-other-window "Switch buffer")
                       (?u aw-flip-window)

                       (?e aw-execute-command-other-window "Execute command other window")

                       (?y aw-split-window-fair "Split fair window")
                       (?- aw-split-window-vert "Split vert window")
                       (?| aw-split-window-horz "Split horz window")

                       (?o delete-other-windows "Delete other windows")

                       (?t transpose-frame)
                       (?= balance-windows)

                       (?? eriks/aw-show-dispatch-help)

                       (?w maximize-window "Maximize window")
                       (?W minimize-window "Minimize window")
                       (?v eriks/shrink-window-dwim "Shrink window")
                       (?^ eriks/enlarge-window-dwim "Enlarge window")

                       (?\M-o other-window-prefix)
                       (?h same-window-prefix)))
  (aw-dispatch-function 'eriks/aw-dispatch)
  :config
  (define-advice set-window-parameter (:filter-args (args) ace-window)
    "Advice to customize the string that appears in the mode line of
`ace-window-display-mode'."
    (cl-block nil
      (cl-destructuring-bind (window parameter value) args
        (unless (eq parameter 'ace-window-path)
          (cl-return args))
        (list window parameter (format "(%s)" value)))))

  (ace-window-display-mode 1)
  ;; NOTE: Makes the thingy `ace-window-display-mode' added to the mode-line dimmed if not
  ;; active. This will unfortunately inhibit the mode to remove itself from the mode line,
  ;; but I always have it one, so it is fine.
  (let ((mlf (default-value 'mode-line-format)))
    (setq-default mode-line-format (cons `(:eval (eriks/mode-line-dim ',(car mlf)))
                                         (cdr mlf))))

  (defun eriks/aw-dispatch (char)
    "Wraps the standard dispatcher to use the selected window if the same
action is pressed twice, akin to something like dd in vim."
    (if (and aw-action
             (eq aw-action (cadr (aw--dispatch-action char))))
        (progn
          (funcall aw-action (selected-window))
          (throw 'done 'exit))
      (aw-dispatch-default char)))

  (defun eriks/aw-show-dispatch-help ()
    "A better variant that display the alist in a table"
    (interactive)
    (let* ((max-width (window-max-chars-per-line (minibuffer-window)))
           (element-width (+ 3 2 35))
           (total (max 1 (/ max-width element-width))))
      (cl-loop for (key fn) in aw-dispatch-alist
               for i from 1
               concat (format "%3s: %-35s" (propertize (key-description (vector key)) 'face 'aw-key-face) fn) into res
               concat (if (= 0 (mod i total)) "\n" " ") into res
               finally (message "%s" (string-trim-right res))))

    ;; Prevent this from replacing any help display
    ;; in the minibuffer.
    (let (aw-minibuffer-flag)
      (mapc #'delete-overlay aw-overlays-back)
      (call-interactively 'ace-window)))

  ;; TODO: not used, remove?
  (defun eriks/aw-kill-buffer (window)
    (unless (window-live-p window)
      (user-error "Got dead window"))
    (kill-buffer (window-buffer window)))

  (defun eriks/aw-quit-window (window &optional kill)
    (with-selected-window window
      (if (derived-mode-p 'magit-mode)
          ;; NOTE: found in `magit-mode-map'
          (magit-mode-bury-buffer kill)
        (quit-window kill))))

  (defun eriks/aw-quit-kill-window (window)
    (eriks/aw-quit-window window t))

  (defun eriks/aw-clone-buffer (window)
    "Clone the current buffer to WINDOW. Alternative to `aw-copy-window'."
    (let ((indirect (clone-indirect-buffer nil nil))
          (switch-to-buffer-obey-display-actions nil))
      (aw-switch-to-window window)
      (switch-to-buffer indirect)))

  (defconst eriks/vertical-resize-amount 4)
  (defconst eriks/horizontal-resize-amount 6)

  (defun eriks/enlarge-window-dwim (&optional window)
    "Make WINDOW larger in some direction by a lagom amount."
    (interactive)
    (setq window (window-normalize-window window))
    (let ((ver (window-max-delta window))
          (hor (window-max-delta window t)))
      (with-selected-window window
        (cond ((> ver 0) (enlarge-window (min ver (floor (frame-total-lines) eriks/vertical-resize-amount))))
              ((> hor 0) (enlarge-window-horizontally (min hor (floor (frame-total-cols) eriks/horizontal-resize-amount))))
              (t (user-error "Window is not resizable"))))))

  (defun eriks/shrink-window-dwim (&optional window)
    "Make WINDOW smaller in some direction by a lagom amount."
    (interactive)
    (setq window (window-normalize-window window))
    (let ((ver (window-min-delta window))
          (hor (window-min-delta window t)))
      (with-selected-window window
        (cond ((> ver 0) (shrink-window (min ver (floor (frame-total-lines) eriks/vertical-resize-amount))))
              ((> hor 0) (shrink-window-horizontally (min hor (floor (frame-total-cols) eriks/horizontal-resize-amount))))
              (t (user-error "Window is not resizable"))))))

  :general-config
  ('global
   "M-o" 'ace-window))

(use-package transpose-frame
  :ensure t)
