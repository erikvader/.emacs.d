(define-derived-mode dirvish-better-directory-view-mode special-mode "Better Dirvish DIRview"
  (setq-local mode-line-format nil
              header-line-format nil
              truncate-lines t
              dirvish--dir-data (dirvish--ht))
  :group 'dirvish
  :interactive nil)

(define-advice dirvish--init-special-buffers (:after (dv) better-dired)
  (let ((better-dired (dirvish--special-buffer 'better-dired dv t))
        (id (dv-id dv)))
    (with-current-buffer better-dired
      (dirvish-better-directory-view-mode)
      (dirvish-prop :dv id))
    (push better-dired (dv-special-buffers dv))))

(dirvish-define-preview better-dired (file)
  "Preview dispatcher for directory FILE."
  (when (file-directory-p file)
    `(better-dired . (let ,(mapcar (lambda (env) `(,(car env) ,(cdr env)))
                                   (remove (cons 'inhibit-message t)
                                           dirvish-preview-environment))
                       (setq insert-directory-program ,insert-directory-program)
                       (setq dired-listing-switches ,dired-listing-switches)
                       (with-current-buffer (dired-noselect ,file)
                         (font-lock-ensure)
                         (let ((str (buffer-string)))
                           (remove-list-of-text-properties 0
                                                           (length str)
                                                           '(mouse-face follow-link help-echo keymap)
                                                           str)

                           ;;NOTE: heavily inspired by `dirvish--hide-dired-header'
                           (let* ((beg (goto-char (point-min)))
                                  (next-file (next-single-char-property-change beg 'dired-filename))
                                  (end (or (and (not next-file) (point-max))
                                           (progn (goto-char next-file) (line-beginning-position)))))
                             (put-text-property beg end 'invisible t str))

                           ;;NOTE: removes the arrows
                           (let ((end (point-max))
                                 (cur (point-min)))
                             (while (/= cur end)
                               (setq cur (next-single-char-property-change cur 'dired-symlink-filename))
                               (goto-char (- cur 4))
                               (when (looking-at-p " -> ")
                                 (put-text-property (- cur 4) cur 'invisible 'dired-hide-details-link str)
                                 (setq cur (next-single-char-property-change cur 'dired-symlink-filename)))))

                           (print str)))))))

(add-to-list 'dirvish-preview-dispatchers 'better-dired)

(cl-defmethod dirvish-preview-dispatch ((recipe (head better-dired)) dv)
  "Fill DV's preview buffer with output of sh command from RECIPE."
  (dirvish--run-shell-for-preview dv recipe))

(define-advice dirvish-shell-preview-proc-s (:around (org proc exitcode) better-dired)
  (if (not (eq (process-get proc 'cmd-info)
               'better-dired))
      (funcall org proc exitcode)
    (when-let* ((dv (dirvish-curr))
                (str (with-current-buffer (process-buffer proc) (buffer-string))))
      (with-current-buffer (dirvish--special-buffer 'better-dired dv t)
        (let (buffer-read-only)
          (erase-buffer)
          (remove-overlays)
          (condition-case err
              (pcase-let ((`(,parsed . ,idx) (read-from-string str)))
                (unless (and (char-or-string-p parsed)
                             (= (1+ idx) (length str)))
                  (error "Parsed %s, up to %s, str len %s" parsed idx (length str)))
                (insert parsed))
            (t
             (print err (current-buffer))
             (insert str)))))
      (kill-buffer (process-buffer proc)))))

(provide 'eriks-dirvish-better-dired-preview)
