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
