(defun eriks/describe-keymap (map)
  "Nicely shows all bindings in map"
  (interactive "sMap: ")
  (when map
    (with-output-to-temp-buffer "*describe keymap*"
      (princ (substitute-command-keys (format "\\{%s}" map))))))

(defun eriks/kill-buffers-of-removed-files ()
  "Prompts to kill every buffer that is visiting a non-existent file."
  (interactive)
  (map-y-or-n-p
   (lambda (buffer)
     (if-let ((filename (buffer-file-name buffer))
              ((not (file-exists-p filename))))
         (format "Kill buffer '%s' visiting removed '%s'? " (buffer-name buffer) filename)))
   #'kill-buffer
   (buffer-list)))

(defun eriks/eval-replace ()
  "Evaluates the sexp at point and replaces it with the result."
  (interactive)
  (when-let* ((b (bounds-of-thing-at-point 'sexp))
              (start (car b))
              (end (cdr b))
              (buf (current-buffer)))
    (let ((res (save-excursion
                 (goto-char start)
                 (eval (read buf)))))
      (kill-region start end)
      (goto-char start)
      (princ res buf))))

(defun quit-window-kill (&optional not-kill window)
  "Same as `quit-window' except it's kill-argument has opposite meaning."
  (interactive "P")
  (quit-window (not not-kill) window))

(defun eriks/scroll-down-half (&optional arg)
  (interactive "^P")
  (scroll-down-command (or arg
                           (/ (window-body-height) 2))))
(put 'eriks/scroll-down-half 'scroll-command t)

(defun eriks/scroll-up-half (&optional arg)
  (interactive "^P")
  (scroll-up-command (or arg
                         (/ (window-body-height) 2))))
(put 'eriks/scroll-up-half 'scroll-command t)

;;TODO: `scroll-other-window' doesn't have the scroll-command property, so these shouldn't
;;either?
(defun eriks/scroll-down-half-other-window (&optional lines)
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (eriks/scroll-down-half lines)))

(defun eriks/scroll-up-half-other-window (&optional lines)
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (eriks/scroll-up-half lines)))

(defun eriks/universal-argument-single ()
  "Runs `universal-argument' without activating
`universal-argument-map'. The implementation is copied from
`universal-argument'"
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (list 4))
  (prefix-command-update))

(defun eriks/universal-argument-double ()
  "Runs `universal-argument' twice without activating
`universal-argument-map'. The implementation is copied from
`universal-argument'"
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (list 16))
  (prefix-command-update))
