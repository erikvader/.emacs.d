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
    "Prints and notifies how many updates are available.
Also removes itself from `package--post-download-archives-hook'"
    (remove-hook 'package--post-download-archives-hook 'eriks/check-and-notify-updates)
    (when-let ((out-pkgs (epl-outdated-packages)))
      (let ((msg (format "There are %d updates available" (length out-pkgs))))
        (message "%s" msg)
        (require 'notifications)
        (notifications-notify :title "Emacs Updates"
                              :body msg))))

  (when (eriks/refresh-package-p 4)
    (message "%s" "refreshing package contents...")
    (add-hook 'package--post-download-archives-hook 'eriks/check-and-notify-updates)
    (package-refresh-contents t)))
