;; -*- lexical-binding: t; -*-

;; NOTE: global markers of killed buffers are buggy
;; https://github.com/emacs-evil/evil/issues/1940
(defun eriks/counsel-evil-marks-get-candidates ()
  (let* ((all-marks (append (seq-filter (lambda (entry)
                                          (not (evil-global-marker-p (car entry))))
                                        evil-markers-alist)
                            (seq-filter (lambda (entry)
                                          (evil-global-marker-p (car entry)))
                                        (default-value 'evil-markers-alist))))
         (sorted (sort all-marks
                       :key #'car
                       :lessp (lambda (a b)
                                (cl-flet ((category (char)
                                            (pcase (get-char-code-property char 'general-category)
                                              ('Ll 1)
                                              ('Lu 2)
                                              (_ 3))))
                                  (value< (list (category a) a)
                                          (list (category b) b))))))
         (mapped (seq-map (lambda (entry)
                            (cl-destructuring-bind (char . val) entry
                              (when-let* ((val (cond
                                                ((symbolp val) nil)
                                                ((functionp val) nil)
                                                ((markerp val)
                                                 ;; NOTE: theres a bug where the marker
                                                 ;; can be invalid. `evil-goto-mark'
                                                 ;; simply throws an error, so I just
                                                 ;; ignore them here.
                                                 (when-let* ((buf (marker-buffer val))
                                                             (pos (marker-position val)))
                                                   (with-current-buffer buf
                                                     (save-excursion
                                                       (save-restriction
                                                         ;; NOTE: `counsel-mark--get-candidates' does this
                                                         (widen)
                                                         (goto-char pos)
                                                         (string-trim-left
                                                          (buffer-substring (line-beginning-position)
                                                                            (line-end-position))))))))
                                                ((consp val)
                                                 (propertize "<killed buffer>" 'face 'warning))
                                                (t (error "Unexpected value when collecting candidates: %s %s" char val)))))
                                (propertize val 'evil-mark entry))))
                          sorted)))
    (seq-filter #'identity mapped)))

(defmacro eriks/counsel-with-mark (candidate &rest body)
  (declare (indent defun))
  `(seq-let (char &rest position) (get-text-property 0 'evil-mark ,candidate)
     (when char ,@body)))

(defun eriks/counsel-evil-marks ()
  "Show all evil markers with ivy.

My own version own `counsel-evil-marks' that works for global markers."
  (interactive)
  (let* ((candidates (eriks/counsel-evil-marks-get-candidates))
         (org-buf (current-buffer))
         (org-positions `((,org-buf . ,(point)))))
    (ivy-read "Evil mark: "
              candidates
              :require-match t
              :update-fn (lambda ()
                           (eriks/counsel-with-mark (ivy-state-current ivy-last)
                             (cond
                              ((markerp position)
                               (counsel--mark-ring-delete-highlight)
                               (let ((buf (marker-buffer position)))
                                 (with-ivy-window
                                   (goto-char (cdr (assoc (current-buffer) org-positions)))
                                   (set-window-buffer nil (set-buffer buf))
                                   (unless (assoc buf org-positions)
                                     (push (cons buf (point)) org-positions))
                                   (goto-char (marker-position position))
                                   (counsel--mark-ring-add-highlight))))
                              ((consp position) nil)
                              (t (error "Unexpected value in update-fn: %s %s" char position)))))
              :unwind (lambda ()
                        (counsel--mark-ring-delete-highlight)
                        (with-ivy-window
                          (goto-char (cdr (assoc (current-buffer) org-positions)))
                          (set-window-buffer nil org-buf)))
              :action #'eriks/counsel-evil-mark-goto-action
              :caller 'eriks/counsel-evil-marks)))

(defun eriks/counsel-evil-mark-goto-action (candidate)
  "Action to go to the current candidate"
  (eriks/counsel-with-mark candidate
    (evil-goto-mark char)))

(defun eriks/counsel-evil-mark-delete-action (candidate)
  "Action to delete the current candidate"
  (eriks/counsel-with-mark candidate
    (evil-delete-marks (string char))
    (when-let* ((win (and (not (eq ivy-exit 'done))
                          (active-minibuffer-window))))
      (ivy-quit-and-run
        (eriks/counsel-evil-marks)))))

(ivy-set-actions
 'eriks/counsel-evil-marks
 '(("d" eriks/counsel-evil-mark-delete-action "delete")))

(defun eriks/counsel-evil-mark-transformer-char (candidate)
  (eriks/counsel-with-mark candidate
    (format "[%s]" (propertize (char-to-string char)
                               'face 'counsel-evil-register-face))))

(defun eriks/counsel-evil-mark-transformer-line (candidate)
  (eriks/counsel-with-mark candidate
    (cond ((markerp position)
           (if-let* ((buf (marker-buffer position))
                     (pos (marker-position position)))
               (with-current-buffer buf
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char pos)
                     (format "%s:%s"
                             (line-number-at-pos)
                             (1+ (current-column))))))
             ""))
          ((consp position)
           (seq-let (filename &rest pos) position
             (number-to-string pos)))
          (t (error "Unknown value in transformer line: %s" position)))))

(defun eriks/counsel-evil-mark-transformer-file (candidate)
  (eriks/counsel-with-mark candidate
    (cond ((markerp position)
           (if-let* ((buf (marker-buffer position)))
               (buffer-name buf)
             ""))
          ((consp position)
           (seq-let (filename &rest pos) position
             filename))
          (t (error "Unknown value in transformer file: %s" position)))))

(defun eriks/counsel-evil-mark-install-ivy-rich ()
  "Call this before enabling `ivy-rich-mode' to install a transformer for
`eriks/counsel-evil-marks'."
  (setf (cl-getf ivy-rich-display-transformers-list 'eriks/counsel-evil-marks)
        '(:columns
          ((eriks/counsel-evil-mark-transformer-char)
           (identity (:width 0.45))
           (eriks/counsel-evil-mark-transformer-line (:width 8 :align right))
           (eriks/counsel-evil-mark-transformer-file)))))

(provide 'eriks-counsel-evil-marks)
