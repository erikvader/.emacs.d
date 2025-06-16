;; compile on install rather than on first load
(setq-default package-native-compile t)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; setup use-package
(use-package use-package
  :demand t
  :custom
  (use-package-always-demand t)
  (use-package-use-theme nil))

(use-package diminish
  :ensure t)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package general
  :ensure t
  :custom
  (general-use-package-emit-autoloads nil))
