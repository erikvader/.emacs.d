
(require 'hi-lock)

(defface eriks/hi-red
  '((t :foreground "black"
       :background "tomato"))
  "red highlight face")

(defface eriks/hi-cyan
  '((t :foreground "black"
       :background "cyan2"))
  "cyan highlight face")

(defvar eriks/highlight-colors
  (list "hi-yellow"
        "hi-pink"
        "hi-blue"
        "eriks/hi-red"
        "eriks/hi-cyan"
        "hi-blue-b"
        "hi-red-b"))

(defun eriks/evil--highlight (reg &optional auto-select-face)
  (let* ((hi-lock-auto-select-face auto-select-face)
         (hi-lock-face-defaults eriks/highlight-colors)
         (face (hi-lock-read-face-name)))
    (hi-lock-face-buffer reg face)))

(defalias 'eriks/evil-unhighlight #'unhighlight-regexp)

(evil-define-operator eriks/evil-highlight (beg end type)
  "Highlight something with hi-lock"
  (interactive "<R>")
  (eriks/evil--highlight (regexp-quote (buffer-substring beg end)) t))

(defun eriks/evil-highlight-regex (&optional arg)
  "Highlights a given regex written in minibuffer interactively.
Uses a default face unless C-u is used."
  (interactive "P")
  (eriks/evil--highlight
   (read-regexp "highlight" "" 'evil-ex-search-history)
   (null (equal arg '(4)))))

(provide 'eriks-evil-highlight)
