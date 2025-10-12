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

;; TODO: remove?
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

;; TODO: add mouse support with `mouse-position' or something. There are sometimes windows
;; that can't be selected, which makes it not possible to call this command there.
(defun eriks/what-face (pos)
  "Show the faces that are used to display the character at point."
  (interactive "d")
  (let* ((overlays (->> (overlays-at pos)
                        (mapcar (lambda (o) (overlay-get o 'face)))
                        (cl-remove-if #'null)))
         (tprop (text-properties-at pos))
         (tface (-> tprop (plist-get 'face)))
         (flface (-> tprop (plist-get 'font-lock-face)))
         (used-face (cond ((and font-lock-mode flface) flface)
                          (tface tface)
                          (t 'default))))
    (cl-flet ((tostring (kind face)
                (format "%s: %s (%s)" kind face (propertize "sample" 'face face))))
      (message "%s" (string-join (cl-list* (tostring "Prop" used-face)
                                           (mapcar (lambda (o) (tostring "Ovrl" o))
                                                   overlays))
                                 ", ")))))
