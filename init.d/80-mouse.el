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
