;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; setup use-package
(use-package use-package
  :demand t
  :custom (use-package-always-demand t))

(use-package diminish
  :ensure t)

(use-package general
  :ensure t)

(use-package pkg-info
  :ensure t)

;; add after-config keywords to `use-package-keywords'
(setq use-package-keywords
      (mapcan (lambda (kw) (cond
                            ((eq kw :config)
                             '(:config :after-config :after-config-hook))
                            ((eq kw :after-config)
                             '())
                            ((eq kw :after-config-hook)
                             '())
                            (t
                             (list kw))))
              use-package-keywords))

(defun eriks/append--after-config (sym)
  "append \"-after-config\" to the symbol SYM and return it as a
  symbol."
  (use-package-as-symbol
   (concat (use-package-as-string sym)
           "-after-config")))

;; Adds a `provide' after the :config part that advertises that this
;; package has had it's config run
(defun use-package-normalize/:after-config-hook (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (if arg
          t
        (use-package-error "lol, i only want t")))))

(defun use-package-handler/:after-config-hook (name keyword activated rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat body `((provide (quote ,(eriks/append--after-config name)))))))

;; Takes several lists where the first element is a quoted symbol of
;; the package to wait for and all other elements are forms to run.
(defun use-package-normalize/:after-config (name keyword args)
  (when (null args)
    (use-package-error (concat (use-package-as-string name)
                               " wants a non-empty list")))
  args)

(defun use-package-handler/:after-config (name keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     (mapcar (lambda (x)
               `(with-eval-after-load
                    (quote ,(eriks/append--after-config
                             (if (and (listp (car x))
                                      (eq (caar x) 'quote)
                                      (symbolp (cadar x)))
                                 (cadar x)
                               (use-package-error "after-config first argument not a quoted symbol"))))
                  ,@(cdr x)))
             args))))
