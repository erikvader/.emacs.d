(evil-define-type exclusive-back
  "Does not include the first character"
  :expand (lambda (beg end)
            (evil-expand (min (1+ beg) end) end 'inclusive)))

(evil-define-motion eriks/evil-backward-word-end-exclusive (count)
  "A version of `evil-backward-word-end' that doesn't include the last
character of the previous word when performing an operator like
`evil-delete'."
  :type exclusive-back
  (evil-backward-word-end count))

(evil-define-motion eriks/evil-backward-WORD-end-exclusive (count)
  "See `eriks/evil-backward-word-end-exclusive'"
  :type exclusive-back
  (evil-backward-WORD-end count))

(provide 'eriks-evil-backward-exclusive)
