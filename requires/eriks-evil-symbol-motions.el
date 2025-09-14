(evil-define-motion eriks/evil-forward-symbol-begin (count)
  "The same as `evil-forward-word-begin', but uses symbols instead of words."
  :type exclusive
  (cl-letf (((symbol-function 'forward-evil-word) (symbol-function 'forward-evil-symbol)))
    (evil-forward-word-begin count)))

(evil-define-motion eriks/evil-forward-symbol-end (count)
  "The same as `evil-forward-word-end', but uses symbols instead of words."
  :type inclusive
  (cl-letf (((symbol-function 'forward-evil-word) (symbol-function 'forward-evil-symbol)))
    (evil-forward-word-end count)))

(evil-define-motion eriks/evil-backward-symbol-begin (count)
  "The same as `evil-backward-word-begin', but uses symbols instead of words."
  :type exclusive
  (cl-letf (((symbol-function 'forward-evil-word) (symbol-function 'forward-evil-symbol)))
    (evil-backward-word-begin count)))

(provide 'eriks-evil-symbol-motions)
