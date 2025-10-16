;;NOTE: recommended by https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq-default switch-to-buffer-obey-display-actions t)

(setq-default fit-window-to-buffer-horizontally t)

(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/theme nil)
  :custom
  (sml/replacer-regexp-list nil)
  :config
  (sml/setup)
  (unless column-number-indicator-zero-based
    (setopt sml/col-number-format (cl-substitute ?C ?c sml/col-number-format)))
  (setopt sml/line-number-format "%4l")
  (setopt sml/show-encoding (cl-substitute ?Z ?z sml/show-encoding))
  (setopt mode-line-percent-position '(-3 "%o")))

(use-package auto-dim-other-buffers
  :ensure t
  :custom
  (auto-dim-other-buffers-dim-on-focus-out nil)
  (auto-dim-other-buffers-affected-faces '((sml/filename mode-line-inactive)
                                           (sml/prefix mode-line-inactive)
                                           (sml/read-only mode-line-inactive)
                                           (sml/global mode-line-inactive)
                                           (sml/modified mode-line-inactive)
                                           (aw-mode-line-face mode-line-inactive)))
  :config
  (auto-dim-other-buffers-mode 1))

(use-package popper
  :ensure t
  :config
  (add-to-list 'popper-reference-buffers (eriks/regexp-quote-all "*Warnings*"))
  ;; NOTE: enabled at the end of initialisation
  ;; (popper-mode 1)
  (popper-echo-mode 1)
  (eriks/defkey-repeat (popper-cycle)
    :keymaps 'popper-mode-map
    :prefix "C-x"
    "<right>" 'popper-cycle
    "<left>" 'popper-cycle-backwards)
  :custom
  (popper-mode-line-position 1)
  (popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :general-config
  ('popper-mode-map
   :prefix "C-x p"
   "t" 'popper-toggle-type
   "k" 'popper-kill-latest-popup)
  ('popper-mode-map
   :prefix "C-x"
   "o" 'popper-toggle))

;;NOTE: is loaded AFTER sml to have the mode line thingy at the front
(use-package ace-window
  :ensure t
  :custom
  ;; NOTE: the global scope doesn't work https://github.com/abo-abo/ace-window/issues/247
  ;; It seems like emacs can't set the focus with my xmonad config
  ;; https://github.com/abo-abo/ace-window/issues/241
  (aw-scope 'frame)
  (aw-display-mode-overlay nil)
  (aw-dispatch-always t)
  (aw-keys '(?s ?d ?f ?g))
  (aw-dispatch-alist '((?x aw-delete-window "Delete window")
                       (?X kill-buffer-and-window "Delete window and kill buffer")
                       (?q eriks/aw-quit-window "Quit window")
                       (?Q eriks/aw-quit-kill-window "Quit window and kill buffer")

                       (?a aw-swap-window "Swap windows")
                       (?m aw-move-window "Move window")

                       (?c eriks/aw-clone-buffer "Clone buffer")

                       (?j aw-switch-buffer-in-window "Switch buffer")
                       (?b aw-switch-buffer-other-window "Switch buffer other window")
                       (?p aw-flip-window)

                       (?e aw-execute-command-other-window "Execute command other window")

                       (?F aw-split-window-fair "Split fair window")
                       (?V aw-split-window-vert "Split vert window")
                       (?H aw-split-window-horz "Split horz window")

                       (?o delete-other-windows "Delete other windows")

                       (?t transpose-frame)

                       (?? aw-show-dispatch-help)

                       (?i fit-window-to-buffer "Fit window")
                       (?w maximize-window "Maximize window")
                       (?v minimize-window "Minimize window")
                       (?l eriks/enlarge-window-dwim "Enlarge window")

                       (?\M-o other-window-prefix)
                       (?h same-window-prefix)))
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

  (defun eriks/aw-quit-window (window)
    (quit-window nil window))

  (defun eriks/aw-quit-kill-window (window)
    (quit-window t window))

  (defun eriks/aw-clone-buffer (window)
    "Clone the current buffer to WINDOW. Alternative to `aw-copy-window'."
    (let ((indirect (clone-indirect-buffer nil nil))
          (switch-to-buffer-obey-display-actions nil))
      (aw-switch-to-window window)
      (switch-to-buffer indirect)))

  (defun eriks/enlarge-window-dwim (&optional window)
    "Make WINDOW larger in some direction by a lagom amount."
    (interactive)
    (setq window (window-normalize-window window))
    (let ((ver (window-max-delta window))
          (hor (window-max-delta window t)))
      (with-selected-window window
        (cond ((> ver 0) (enlarge-window (min ver (floor (frame-total-lines) 3))))
              ((> hor 0) (enlarge-window-horizontally (min hor (floor (frame-total-cols) 6))))
              (t (user-error "Window is not resizable"))))))

  (define-advice aw-show-dispatch-help (:around (f) non-displayable)
    "Remove non-characters in `aw-dispatch-alist' to make this help message work."
    (let* ((test (lambda (ele) (-> ele car characterp)))
           (aw-dispatch-alist (cl-remove-if-not test aw-dispatch-alist))
           ;;TODO: display these somehow
           (non-displayable (cl-remove-if test aw-dispatch-alist)))
      (funcall f)))

  :general-config
  ('global
   "M-o" 'ace-window))

(use-package transpose-frame
  :ensure t)
