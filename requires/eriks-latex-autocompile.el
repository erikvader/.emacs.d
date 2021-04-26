(require 'seq)

(defcustom eriks/latex-autocompile-latex-modes '(latex-mode LaTeX-mode plain-tex-mode)
  "Modes where `eriks/latex-autocompile-mode' can be enabled in.")

(defcustom eriks/latex-autocompile-child-modes '(bibtex-mode)
  "Modes where `eriks/latex-autocompile-mode' can be enabled in, but only in child mode.")

(defvar-local eriks/latex-autocompile-command nil
  "TeX command to auto compile with")

(defvar-local eriks/latex-autocompile-parent nil
  "Buffer object to another buffer where the command in
`eriks/latex-autocompile-command' will be run")

(defun eriks/latex-compile-on-save ()
  "Auto compile on save if enabled.
see `eriks/latex-autocompile-mode'"
  (unless eriks/latex-autocompile-mode
    (user-error "compile-on-save run without mode being active"))

  (cond (eriks/latex-autocompile-command
         (TeX-command eriks/latex-autocompile-command 'TeX-active-master))
        ((and eriks/latex-autocompile-parent
              (or (buffer-live-p eriks/latex-autocompile-parent)
                  (user-error "parent buffer no longer exists")))
         (with-current-buffer eriks/latex-autocompile-parent
           (when eriks/latex-autocompile-parent
             (user-error "parent shouldn't be set"))
           (eriks/latex-compile-on-save)))
        (t
         (user-error "neither command nor parent is set"))))

(defun eriks/latex-active-buffers ()
  "Returns a list of potential parent buffers"
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (and
                   (bound-and-true-p eriks/latex-autocompile-mode)
                   (not eriks/latex-autocompile-parent)
                   eriks/latex-autocompile-command)))
              (buffer-list)))

(defun eriks/latex-maybe-prompt-parent (&optional required)
  "Prompts for a parent buffer if any exist and set variables accordingly"
  (when-let ((active (mapcar (lambda (buf) (cons (buffer-name buf) buf))
                             (eriks/latex-active-buffers)))
             (input (completing-read (concat "Set parent"
                                             (and (not required) " (empty for none)")
                                             ": ")
                                     active
                                     nil
                                     (and required t)))
             ((not (string-empty-p input)))
             (selected (cdr (assoc input active #'string-equal))))
    (setq eriks/latex-autocompile-parent selected)
    t))

(defun eriks/latex-prompt-new-root ()
  "Prompts for tex command to use"
  (setq eriks/latex-autocompile-command (TeX-command-query (TeX-master-file nil nil t)))
  t)

(defmacro eriks/on-signal (handler &rest body)
  `(condition-case err
       ,@body
     (t (progn ,handler
               (signal (car err) (cdr err))))))

(define-minor-mode eriks/latex-autocompile-mode
  "Toggles whether the document should be compiled after each save or not.
Queries the command to use when toggling on.

Dependant files can be linked to a master file, queried if applicable."
  nil " LA" nil
  (cl-flet ((is-latex-mode () (and (seq-some #'derived-mode-p eriks/latex-autocompile-latex-modes)
                                   (fboundp 'TeX-command-query)
                                   (fboundp 'TeX-master-file)))
            (is-child-mode () (seq-some #'derived-mode-p eriks/latex-autocompile-child-modes))
            (add-save-hook () (add-hook 'after-save-hook 'eriks/latex-compile-on-save nil t)))
    (if eriks/latex-autocompile-mode
        (eriks/on-signal
         (setq eriks/latex-autocompile-mode nil)
         (cond ((and (is-child-mode)
                     (or (eriks/latex-maybe-prompt-parent t)
                         (user-error "no parent buffer found")))
                (add-save-hook))
               ((and (is-latex-mode)
                     (or (eriks/latex-maybe-prompt-parent)
                         (eriks/latex-prompt-new-root)))
                (add-save-hook))
               (t
                (user-error "must be in latex-mode with auctex installed or in bibtex-mode"))))
      (remove-hook 'after-save-hook 'eriks/latex-compile-on-save t)
      (setq eriks/latex-autocompile-command nil
            eriks/latex-autocompile-parent nil))))

(provide 'eriks-latex-autocompile)
