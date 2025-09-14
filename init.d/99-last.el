;; NOTE: various modes want to set `popper-reference-buffers' and stuff, but those
;; settings are only used when activating the mode, so it is activated here at the very
;; end.
(when (functionp 'popper-mode)
  (popper-mode 1))
