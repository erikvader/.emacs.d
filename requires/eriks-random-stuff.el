(defun eriks/spawn-external-terminal ()
  "Spawns a terminal emulator outside of Emacs."
  (interactive)
  (call-process "setsid" nil 0 nil "st"))

(provide 'eriks-random-stuff)
