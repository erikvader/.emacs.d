;;NOTE: recommended by https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq-default switch-to-buffer-obey-display-actions t)

(setq-default fit-window-to-buffer-horizontally t)

(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/theme nil)
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
                                           (aw-mode-line-face mode-line-inactive)))
  :config
  (auto-dim-other-buffers-mode 1))

(use-package popper
  :ensure t
  :config
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
  (popper-reference-buffers '("\\*Warnings\\*$" compilation-mode))
  :general-config
  ('popper-mode-map
   :prefix "C-x"
   "o" 'popper-toggle))

;;NOTE: is loaded AFTER sml to have the mode line thingy at the front
(use-package ace-window
  :ensure t
  :custom
  (aw-dispatch-always t)
  (aw-keys '(?s ?d ?f ?g ?h ?k ?l))
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

  ;; TODO: `clone-indirect-buffer' on c?
  (add-to-list 'aw-dispatch-alist '(?i fit-window-to-buffer "Fit window"))
  (add-to-list 'aw-dispatch-alist '(?w maximize-window "Maximize window"))
  (add-to-list 'aw-dispatch-alist '(?q eriks/aw-quit-window "Quit window"))
  (add-to-list 'aw-dispatch-alist '(?Q eriks/aw-quit-kill-window "Quit and kill window"))
  (add-to-list 'aw-dispatch-alist '(?\M-o other-window-prefix))

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
