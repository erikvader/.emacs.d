(use-package avy
  :ensure t
  :custom
  (avy-keys '(?h ?g ?j ?f ?k ?d ?l ?s))
  (avy-timeout-seconds nil) ;; no timeout ("infinite" timeout)
  (avy-all-windows nil)
  (avy-background t)
  (avy-highlight-first nil)
  (avy-style 'at-full)
  :config
  (add-to-list 'avy-orders-alist '(avy-goto-char-timer . avy-order-closest))
  (defun eriks/avy-goto-char-timer-advice (f &rest rest)
    "Advice to make SPC do the same thing as RET in avy timer."
    (cl-letf* ((old-read-char (symbol-function #'read-char))
               ((symbol-function #'read-char) (lambda (&rest re)
                                                (let ((key (apply old-read-char re)))
                                                  (if (= key 32)
                                                      13
                                                    key)))))
      (apply f rest)))
  (advice-add 'avy-goto-char-timer :around #'eriks/avy-goto-char-timer-advice)
  (eriks/leader-def 'motion
    "SPC" 'avy-goto-char-timer))

(use-package evil-easymotion
  ;; :config
  ;; (evilem-make-motion
  ;;  evilem-motion-repeat-find-char-inline #'evil-repeat-find-char
  ;;  :bind ((evil-cross-lines nil)))
  ;; (evilem-make-motion
  ;;  evilem-motion-repeat-find-char-reverse-inline #'evil-repeat-find-char-reverse
  ;;  :bind ((evil-cross-lines nil)))
  :config
  (eriks/leader-def 'motion
    "j" #'evilem-motion-next-line
    "k" #'evilem-motion-previous-line
    ;;TODO: i never (barely) use these, remove?
    ;; "w" #'evilem-motion-forward-word-begin
    ;; "W" #'evilem-motion-forward-WORD-begin
    ;; "e" #'evilem-motion-forward-word-end
    ;; "E" #'evilem-motion-forward-WORD-end
    ;; "b" #'evilem-motion-backward-word-begin
    ;; "B" #'evilem-motion-backward-WORD-begin
    ;; "t" #'evilem-motion-find-char-to-inline
    ;; "T" #'evilem-motion-find-char-to-backward-inline
    ;; "f" #'evilem-motion-find-char-inline
    ;; "F" #'evilem-motion-find-char-backward-inline
    ;; "+" #'evilem-motion-next-line-first-non-blank
    ;; "-" #'evilem-motion-previous-line-first-non-blank
    ;; "," #'evilem-motion-repeat-find-char-reverse-inline
    ;; ";" #'evilem-motion-repeat-find-char-inline
    ))
