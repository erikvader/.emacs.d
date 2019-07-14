;; general
(setq-default standard-indent 3)

;; all of them
;; font-locks
(defun eriks/add-marker-font-locks ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\):" 1 'fixme-face t)
     ("\\<\\(TODO\\):" 1 'todo-face t)
     ("\\<\\(NOTE\\):" 1 'note-face t))))
(add-hook 'prog-mode-hook #'eriks/add-marker-font-locks)

;; lisp
(use-package eldoc
  :diminish eldoc-mode)
