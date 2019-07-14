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
  :ensure t
  :config
  (defmacro eriks/general-def-each (&rest lists)
    "Runs `general-def' with arguments from each list in LISTS."
    `(progn
       ,@(mapcar (lambda (y) (cons 'general-def y)) body)))
  (defmacro eriks/general-def-evil (&rest body)
    "Runs `eriks/general-def-each' with BODY, but waits for evil to load."
    `(with-eval-after-load 'evil
       (eriks/general-def-each
        ,@body))))

