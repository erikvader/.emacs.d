(use-package frames-only-mode
  :ensure t
  :init
  (defun eriks/frames-only-use-window-funcs (fun)
    "add FUN to `frames-only-mode-use-window-functions'"
    (when (featurep 'frames-only-mode)
      (add-to-list 'frames-only-mode-use-window-functions fun)
      ;; have to do this manually if mode already is activated
      (when frames-only-mode
        (advice-add fun :around #'frames-only-mode-advice-use-windows))))
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
