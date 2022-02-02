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
  (advice-add 'avy-goto-char-timer :around #'eriks/avy-goto-char-timer-advice))

(use-package evil-easymotion
  :after (:and evil avy)
  :general
  ('operator
   "J" 'evilem-motion-next-line-first-non-blank
   "K" 'evilem-motion-previous-line-first-non-blank)
  ('motion
   :prefix eriks/leader
   "w" #'evilem-motion-forward-word-begin
   "W" #'evilem-motion-forward-WORD-begin
   "e" #'evilem-motion-forward-word-end
   "E" #'evilem-motion-forward-WORD-end
   "b" #'evilem-motion-backward-word-begin
   "B" #'evilem-motion-backward-WORD-begin
   "t" #'evilem-motion-find-char-to
   "T" #'evilem-motion-find-char-to-backward
   "f" #'evilem-motion-find-char
   "F" #'evilem-motion-find-char-backward
   "j" #'evilem-motion-next-line
   "k" #'evilem-motion-previous-line))

(use-package eriks-evil-avy-motions
  :after (:and avy evil evil-easymotion)
  :general
  ('motion
   "+" 'evil-eriks/avy-goto-line-first-non-blank
   "," 'avy-goto-char-in-line)
  ('motion
   :prefix eriks/leader
   "SPC" 'avy-goto-char-timer
   "," 'avy-goto-char))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :general
  ('normal
   "C-x o" 'ace-window))
