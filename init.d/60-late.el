;; (use-package vdiff-magit
;;   :ensure t
;;   :config
;;   (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
;;   (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
;;   (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
;;   (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)
;;   :general
;;   ('magit-mode-map
;;    "e" 'vdiff-magit-dwim
;;    "E" 'vdiff-magit))
