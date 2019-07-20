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

(defun eriks/add-to-lists (list items)
  "Same as `add-to-list' except that ITEMS is a list of elements to be
added."
  (mapc (lambda (i)
          (add-to-list list i))
        items))

