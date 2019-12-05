(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/theme nil)
  :config
  (sml/setup))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode 1)
  :general
  ('projectile-mode-map
   :prefix "C-c"
   "p" 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-expand-only-for-last-commands '(self-insert-command org-self-insert-command))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ibuffer
  :defer t
  :general
  ("C-x C-b" 'ibuffer))

(use-package dumb-jump
  :ensure t
  :defer t
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (advice-add 'dumb-jump-go :before (cl-defun eriks/dumb-jump-go-evil-advice (&rest r)
                                      (evil-set-jump)))
  :general
  ('normal
   "gd" 'dumb-jump-go
   "gD" 'dumb-jump-quick-look))

(use-package eriks-random-stuff
  :general
  ('normal
   :prefix eriks/leader
   "o" 'eriks/spawn-external-terminal))

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-page)
  :config
  (pdf-tools-install :no-query-p t)
  (defun eriks/pdf-goto-page (arg)
    "Goto first page if no prefix argument, otherwise goto page ARG."
    (interactive "p")
    (pdf-view-goto-page arg))
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  :general
  ('pdf-view-mode-map
   [remap swiper] 'isearch-forward
   "C-s" 'isearch-forward
   "/" 'isearch-forward
   "O" 'pdf-occur
   "q" 'quit-window-kill
   "w" 'pdf-view-fit-page-to-window
   "W" 'pdf-view-fit-width-to-window
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page
   "." 'pdf-view-next-page-command
   "," 'pdf-view-previous-page-command
   "H" 'image-bob
   "L" 'image-eob
   "y" 'pdf-view-kill-ring-save
   "g" 'eriks/pdf-goto-page
   "G" 'pdf-view-last-page
   "n" 'pdf-history-forward
   "p" 'pdf-history-backward
   "C-o" 'pdf-history-backward
   "C-i" 'pdf-history-forward)
  ('pdf-links-minor-mode-map
   "f" 'pdf-links-action-perform
   "F" 'pdf-links-isearch-link)
  ('isearch-mode-map
   "<escape>" 'isearch-abort))

(use-package frames-only-mode
  :ensure t
  :after-config-hook t
  :config
  (defun eriks/frames-only-use-window-funcs (fun)
    "add FUN to `frames-only-mode-use-window-functions'"
    (when (featurep 'frames-only-mode)
      (add-to-list 'frames-only-mode-use-window-functions fun)
      ;; have to do this manually if mode already is activated
      (when frames-only-mode
        (advice-add fun :around #'frames-only-mode-advice-use-windows))))
  (defun kill-buffer-and-frame ()
    "Kills the current buffer, if successful then delete the frame."
    (interactive)
    (when (and
           (buffer-modified-p)
           (y-or-n-p "Current buffer is modified, save?"))
      (save-buffer))
    (when (kill-buffer)
      (delete-frame)))
  (defun eriks/window-deletable-p (&optional window)
    "A copy of `window-deletable-p' where the restriction of
needing more than one frame is removed. This is to play nice with
`frames-only-mode' and emacsclient where it is okey to close the
last frame.

Since 2019-12-05, Emacs version 26.3"
    (setq window (window-normalize-window window))

    (unless (or ignore-window-parameters
                (eq (window-parameter window 'delete-window) t))
      (when (window-parameter window 'window-atom)
        (setq window (window-atom-root window))))

    (let ((frame (window-frame window)))
      (cond
       ((frame-root-window-p window)
        (unless (or ;; (eq frame (next-frame frame 0)) ;;NOTE: removed this!!
                    (catch 'other
                      (dolist (other (frame-list))
                        (when (and (not (eq other frame))
                                   (eq (window-frame (minibuffer-window other))
                                       frame))
                          (throw 'other t))))
                    (let ((minibuf (active-minibuffer-window)))
                      (and minibuf (eq frame (window-frame minibuf)))))
          'frame))
       ((window-minibuffer-p window)
        nil)
       ((or ignore-window-parameters
            (not (eq window (window-main-window frame))))
        t))))
  (advice-add 'window-deletable-p :override 'eriks/window-deletable-p)
  (frames-only-mode 1)
  :custom
  (frames-only-mode-use-window-functions
   '(calendar
     report-emacs-bug
     checkdoc-show-diagnostics
     checkdoc
     Custom-newline ;; customize actions window
     ))
  (frames-only-mode-kill-frame-when-buffer-killed-buffer-list
   '("*RefTeX Select*"
     "*Help*"
     "*Popup Help*"
     "*Completions*"
     "widget-choose" ;; actually close customize's action window
     ))
  (frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops nil)
  :general
  ("C-x C-0" 'kill-buffer-and-frame))

(use-package wgrep
  :ensure t)
