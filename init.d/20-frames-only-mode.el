(use-package frames-only-mode
  :ensure t
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
