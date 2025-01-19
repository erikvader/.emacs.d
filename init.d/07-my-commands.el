(defun eriks/describe-keymap (map)
  "Nicely shows all bindings in map"
  (interactive "sMap: ")
  (when map
    (with-output-to-temp-buffer "*describe keymap*"
      (princ (substitute-command-keys (format "\\{%s}" map))))))

(defun eriks/diction-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "diction -L en_GB -bs"))

(defun eriks/recompile-elpa ()
  "Compile every .el-file in 'elpa'."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0 t))

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

;;TODO: use sexp before point instead
(defun eriks/eval-replace ()
  "Evaluates the sexp at point and replaces it with the result."
  (interactive)
  (let ((b (bounds-of-thing-at-point 'sexp))
        (buf (current-buffer)))
    (when (and b buf)
      (goto-char (car b))
      (princ (eval (read buf)) buf)
      (kill-region (car b) (cdr b)))))

(defun quit-window-kill (&optional not-kill window)
  "Same as `quit-window' except it's kill-argument has opposite meaning."
  (interactive "P")
  (quit-window (not not-kill) window))
