;; general
(setq-default standard-indent 3)

;; all of them
;; lisp
(use-package eldoc
  :diminish eldoc-mode)

;; latex
;;(require 'latex)
;;(require 'auctex-latexmk)
;;(setq auctex-latexmk-inherit-TeX-PDF-mode t)
;;(auctex-latexmk-setup)
;;
;;;; default viewer
;;(setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")
;;
;;(define-key LaTeX-mode-map (kbd "C-c e") 'TeX-error-overview)
;;
;;(defvar eriks/latex-autocompile nil "Whether the current buffer's after-save-hook should compile the document")
;;(make-variable-buffer-local 'eriks/latex-autocompile)
;;(defvar eriks/latex-autocompile-command nil "TeX command to auto compile with")
;;(make-variable-buffer-local 'eriks/latex-autocompile-command)
;;
;;(defun eriks/latex-autocompile-toggle ()
;;  "Toggles whether the document should be compiled after each save or not.
;;Queries the command to use when toggling on."
;;  (interactive)
;;  (setq eriks/latex-autocompile-command nil)
;;  (when (not eriks/latex-autocompile)
;;    (setq eriks/latex-autocompile-command (TeX-command-query (TeX-master-file nil nil t))))
;;  (setq eriks/latex-autocompile (and (not eriks/latex-autocompile)
;;                                     eriks/latex-autocompile-command))
;;  (message "auto compilation is %s" (if eriks/latex-autocompile "on" "off")))
;;
;;(define-key LaTeX-mode-map (kbd "C-c a") 'eriks/latex-autocompile-toggle)
;;
;;(defun eriks/latex-compile-on-save ()
;;  "Auto compile on save if enabled.
;;see `eriks/latex-autocompile-toggle'"
;;  (when (and eriks/latex-autocompile
;;             eriks/latex-autocompile-command)
;;    (TeX-command eriks/latex-autocompile-command 'TeX-active-master)))
;;
;;(defun LaTeX-mode-hook-fun ()
;;  ;; (modify-syntax-entry ?$ "\"" LaTeX-mode-syntax-table) ;;make $ act like string so smartparens can navigate with it.
;;  (add-hook 'after-save-hook 'eriks/latex-compile-on-save nil t)
;;  (dolist (i '(paragraph-start paragraph-separate))
;;    (set i (default-value i)))
;;  (TeX-source-correlate-mode 1)
;;  (run-hooks 'prog-mode-hook))
;;(add-hook 'LaTeX-mode-hook 'LaTeX-mode-hook-fun)
