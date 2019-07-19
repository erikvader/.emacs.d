;; general
(setq-default standard-indent 3)

;; all of them
;; lisp
(use-package eldoc
  :diminish eldoc-mode)

(use-package rainbow-delimiters
  :ensure t
  :ghook 'prog-mode-hook)

(defun eriks/prog-mode-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'eriks/prog-mode-show-trailing-whitespace)

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

;; (defun latex-surround-env ()
;;   (let ((ename (or (read-from-minibuffer "environment: ") "")))
;;     (cons (format "\\begin{%s}" ename) (format "\\end{%s}" ename))))

;; (defun LaTeX-mode-hook-textobj-fun ()
;;   ;; (add-to-list 'evil-surround-pairs-alist '(?f . erik-evil-surround-latex-macro))
;;   (add-to-list 'evil-surround-pairs-alist '(?$ . ("$" . "$")))
;;   (add-to-list 'evil-surround-pairs-alist '(?m . latex-surround-env))
;;   (evil-define-inner-local-textobject "$" 'evil-latex-textobjects-inner-dollar)
;;   (evil-define-outer-local-textobject "$" 'evil-latex-textobjects-a-dollar)
;;   (evil-define-inner-local-textobject "\\" 'evil-latex-textobjects-inner-math)
;;   (evil-define-outer-local-textobject "\\" 'evil-latex-textobjects-a-math)
;;   (evil-define-inner-local-textobject "f" 'evil-latex-textobjects-inner-macro)
;;   (evil-define-outer-local-textobject "f" 'evil-latex-textobjects-a-macro)
;;   (evil-define-inner-local-textobject "m" 'evil-latex-textobjects-inner-env)
;;   (evil-define-outer-local-textobject "m" 'evil-latex-textobjects-an-env))
;; (add-hook 'LaTeX-mode-hook 'LaTeX-mode-hook-textobj-fun)
