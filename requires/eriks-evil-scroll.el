(defun eriks/evil-scroll-up ()
  "The same as 'Hzz'"
  (interactive)
  (evil-window-top nil)
  (evil-scroll-line-to-center nil))

;;TODO: these don't work in line visual mode
(defun eriks/evil-scroll-down ()
  "The same as 'Lzz'"
  (interactive)
  (evil-window-bottom nil)
  (evil-scroll-line-to-center nil))

(provide 'eriks-evil-scroll)
