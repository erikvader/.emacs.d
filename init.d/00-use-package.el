;; compile on install rather than on first load
(setq-default package-native-compile t)

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

(use-package epl
  :ensure t
  :config
  (defun eriks/check-and-notify-updates ()
    "Prints and notifies how many updates are available."
    (let* ((out-pkgs (epl-outdated-packages))
           (msg (format "There are %d updates available" (length out-pkgs))))
      (when out-pkgs
        (message "%s" msg)
        (require 'notifications)
        (notifications-notify :title "Emacs Updates"
                              :body msg))))

  (when (eriks/refresh-package-p 4)
    (message "%s" "refreshing package contents...")
    (package-refresh-contents))
  (eriks/check-and-notify-updates))
