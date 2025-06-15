;; color theme
(defface todo-face '((t . (:inherit default))) "face for TODO")
(defface fixme-face '((t . (:inherit default))) "face for FIXME")
(defface note-face '((t . (:inherit default))) "face for NOTE")
(defface tab-face '((t . (:inherit default))) "face for tabs")
(load-theme 'dracula t nil)

;; Easier to remember aliases
(defalias 'theme-enable #'enable-theme)
(defalias 'theme-disable #'disable-theme)

;; font
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 14"))
(set-fontset-font t 'symbol "Noto Color Emoji")

;; column line
(add-hook 'prog-mode-hook
          (cl-defun eriks/activate-display-fill-column ()
            (setq display-fill-column-indicator t)
            (setq display-fill-column-indicator-character ?│)))

;; font-locks
(defun eriks/add-marker-font-locks ()
  "Makes FIXME, TODO and NOTE get highlighted in current buffer"
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\):" 1 'fixme-face t)
     ("\\<\\(TODO\\):" 1 'todo-face t)
     ("\\<\\(NOTE\\):" 1 'note-face t))))

(add-hook 'prog-mode-hook #'eriks/add-marker-font-locks)

(defun eriks/add-tab-font-lock ()
  "Display tabs in the current buffer using `tab-face'."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\t" 0 'tab-face t))))

(add-hook 'prog-mode-hook #'eriks/add-tab-font-lock)
(add-hook 'text-mode-hook #'eriks/add-tab-font-lock)

(defun eriks/prog-mode-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'eriks/prog-mode-show-trailing-whitespace)
