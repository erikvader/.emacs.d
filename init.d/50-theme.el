;; color theme
(defface todo-face '((t . (:inherit default))) "face for TODO")
(defface fixme-face '((t . (:inherit default))) "face for FIXME")
(defface note-face '((t . (:inherit default))) "face for NOTE")
(load-theme 'dracula t nil)

;; font
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 14"))

;; font-locks
(defun eriks/add-marker-font-locks ()
  "Makes FIXME, TODO and NOTE get highlighted in current buffer"
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\):" 1 'fixme-face t)
     ("\\<\\(TODO\\):" 1 'todo-face t)
     ("\\<\\(NOTE\\):" 1 'note-face t))))

(add-hook 'prog-mode-hook #'eriks/add-marker-font-locks)
