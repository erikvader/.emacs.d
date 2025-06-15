;; Make clicks of the mouse only set window focus, not move the point within it.
(general-unbind '(global motion insert normal visual replace emacs operator)
  [mouse-1]
  [drag-mouse-1]
  [down-mouse-1]
  [mouse-3]
  [drag-mouse-3]
  [down-mouse-3])

(general-def 'global
  [mouse-1] 'mouse-select-window
  [mouse-3] 'mouse-set-point)

;; Instruct emacs how my xmonad handles the mouse cursor, i.e., the mouse follows focus
(setq-default focus-follows-mouse t)
