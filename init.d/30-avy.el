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
  ;; NOTE: these are copies of the normal ones, but they respect `track-eol' and
  ;; `evil-track-eol' by allowing the `temporary-goal-column' to be
  ;; `most-positive-fixnum'.
  (defun eriks/evilem-change-line-goal-column ()
    (if (and (eq temporary-goal-column most-positive-fixnum)
             (memq last-command '(next-line previous-line)))
        most-positive-fixnum
      (current-column)))
  (evilem-make-motion evilem-motion-next-line2 #'next-line
                      :pre-hook (setq evil-this-type 'line)
                      :bind ((temporary-goal-column (eriks/evilem-change-line-goal-column))
                             (line-move-visual nil)))
  (evilem-make-motion evilem-motion-previous-line2 #'previous-line
                      :pre-hook (setq evil-this-type 'line)
                      :bind ((temporary-goal-column (eriks/evilem-change-line-goal-column))
                             (line-move-visual nil)))
  (eriks/leader-def 'motion
    "j" #'evilem-motion-next-line2
    "k" #'evilem-motion-previous-line2
    "+" #'evilem-motion-next-line-first-non-blank
    "-" #'evilem-motion-previous-line-first-non-blank))
