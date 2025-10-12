(use-package avy
  :ensure t
  :custom
  (avy-keys '(?h ?g ?j ?f ?k ?d ?l ?s))
  (avy-timeout-seconds nil) ;; no timeout ("infinite" timeout)
  (avy-background t)
  (avy-highlight-first nil)
  (avy-style 'at-full)
  (avy-all-windows nil)
  :config
  (add-to-list 'avy-orders-alist '(avy-goto-char-timer . avy-order-closest))
  (define-advice avy-goto-char-timer (:around (f &rest rest) eriks)
    "Advice to make SPC do the same thing as RET in avy timer."
    (cl-letf* ((avy-all-windows t)
               (old-read-char (symbol-function #'read-char))
               ((symbol-function #'read-char) (lambda (&rest re)
                                                (let ((key (apply old-read-char re)))
                                                  (if (= key 32)
                                                      13
                                                    key)))))
      (apply f rest)))
  (eriks/leader-def 'motion
    "SPC" 'avy-goto-char-timer)
  :general-config
  ('global
   "C-'" 'avy-goto-line))

;;NOTE: This package contains bugs for motions like w, but I don't use those.
;;https://github.com/PythonNut/evil-easymotion/pull/71
;;https://github.com/erikvader/evil-easymotion/tree/eriks
(use-package evil-easymotion
  :ensure t
  :config
  (evilem-make-motion eriks/evilem-next-line-last-non-blank #'eriks/evil-next-line-last-non-blank)
  (evilem-make-motion eriks/evilem-previous-line-last-non-blank #'eriks/evil-previous-line-last-non-blank)
  (eriks/leader-def 'motion
    "j" #'evilem-motion-next-line
    "k" #'evilem-motion-previous-line
    "J" #'eriks/evilem-next-line-last-non-blank
    "K" #'eriks/evilem-previous-line-last-non-blank
    "+" #'evilem-motion-next-line-first-non-blank
    "-" #'evilem-motion-previous-line-first-non-blank))
