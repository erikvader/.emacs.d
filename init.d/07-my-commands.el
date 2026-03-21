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
         ;; NOTE: copied from `describe-char'
         (char (char-after pos))
         (hardcoded-face
          (cond
           ((and show-trailing-whitespace
                 (save-excursion (goto-char pos)
                                 (looking-at-p "[ \t]+$")))
            'trailing-whitespace)
           ((and nobreak-char-display char
                 (> char 127)
                 (eq (get-char-code-property char 'general-category) 'Zs))
            'nobreak-space)
           ((and nobreak-char-display char
                 (memq char '(#xad #x2010 #x2011)))
            'escape-glyph)
           ((and (< char 32) (not (memq char '(9 10))))
            'escape-glyph)))
         (tprop (text-properties-at pos))
         (tface (-> tprop (plist-get 'face)))
         (flface (-> tprop (plist-get 'font-lock-face)))
         (used-face (cond ((and font-lock-mode flface) flface)
                          (tface tface)
                          (t 'default))))
    (cl-flet ((tostring (kind face)
                (list (format "%s: %s (%s)" kind face (propertize "sample" 'face face)))))
      (message "%s" (string-join (append (tostring "Prop" used-face)
                                         (when hardcoded-face (tostring "Hard" hardcoded-face))
                                         (mapcan (apply-partially #'tostring "Ovrl") overlays))
                                 ", ")))))

(defun eriks/eval-replace-last-sexp (&optional arg)
  "Like `eval-last-sexp', but the sexp is replaced with the result."
  (interactive "P")
  (let ((fmt (if arg "%s" "%S"))
        ;; NOTE: `eval-last-sexp' uses `elisp--preceding-sexp', and `backward-kill-sexp'
        ;; uses `forward-sexp', so it is not the exact same function that finds the
        ;; preceding sexp, which I don't like, since there _could_ be differences. But I
        ;; must admit that it is probably fine, but I don't like it.
        (value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format fmt value))))
