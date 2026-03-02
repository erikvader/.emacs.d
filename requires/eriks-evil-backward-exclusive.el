(evil-define-type exclusive-back
  "Does not include the first and last character"
  :expand (lambda (beg end)
            (evil-expand (min (1+ beg) end) end 'exclusive)))

(evil-define-motion eriks/evil-backward-word-end-exclusive (count)
  "A version of `evil-backward-word-end' that doesn't include the last
character of the previous word when performing an operator like
`evil-delete'.

This motion is nice for situations like:

frobnicate       |asd

where the cursor is before the second word and the goal is to delete all
whitespace behind it, which can be done with dge. The emacs command
`cycle-spacing' can alternatively be used to achieve the same thing."
  :type exclusive-back
  (evil-backward-word-end count))

(evil-define-motion eriks/evil-backward-WORD-end-exclusive (count)
  "See `eriks/evil-backward-word-end-exclusive'"
  :type exclusive-back
  (evil-backward-WORD-end count))

(provide 'eriks-evil-backward-exclusive)
