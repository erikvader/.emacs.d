(defcustom eriks/use-frames-only-mode t
  "Whether `frames-only-mode' should be loaded and enabled. Other
kinds of config to optimize for a single frame are loaded if
nil.")

(defun eriks/frames-only-use-window-funcs (&rest funs)
  "add FUN to `frames-only-mode-use-window-functions'"
  (when (featurep 'frames-only-mode)
    (dolist (fun funs)
      (add-to-list 'frames-only-mode-use-window-functions fun)
      ;; have to do this manually if mode already is activated
      (when frames-only-mode
        (advice-add fun :around #'frames-only-mode-advice-use-windows)))))

(defun eriks/frames-only-kill-frame-if (&rest bufs)
  "add FUN to `frames-only-mode-kill-frame-when-buffer-killed-buffer-list'"
  (when (featurep 'frames-only-mode)
    (dolist (buf bufs)
      (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list buf))))

;;TODO: det är något konstigt med att en buffer som redan visas i en annan frame och får
;;fokus inte får fokus av window managern. https://github.com/davidshepherd7/frames-only-mode/issues/17
;;problem med xmonad?
;;TODO: use `pop-global-mark' when in emacs state?
;;TODO: use evil in more places to get access to C-o?
;;TODO: motion state everywhere?
;;TODO: https://karthinks.com/software/emacs-window-management-almanac/
;;TODO: popper
;;TODO: shackle
;;TODO: https://github.com/axgfn/edwina
;;TODO: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(use-package frames-only-mode
  :if eriks/use-frames-only-mode
  :ensure t
  :config
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
`frames-only-mode' and emacsclient where it is okay to close the
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

  (eriks/frames-only-kill-frame-if
   "widget-choose" ;; actually close customize's action window
   )

  (eriks/frames-only-use-window-funcs
   #'read-multiple-choice ;; For the help buffer that can be shown
   #'display-warning ;; logs from byte compilation for example
   #'Custom-newline ;; customize actions window
   )

  (frames-only-mode 1)
  :general-config
  ("C-x C-0" 'kill-buffer-and-frame))
