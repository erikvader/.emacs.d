(defmacro same-buffer (&rest BODY)
  "Executes BODY while forcing `switch-to-buffer-other-window' and
`display-buffer' to always open in the current window, if possible
(see `switch-to-buffer' and its last argument FORCE-SAME-WINDOW
when set to nil)."
  `(cl-letf (((symbol-function 'switch-to-buffer-other-window)
              (lambda (BUFFER-OR-NAME &rest args)
                (switch-to-buffer BUFFER-OR-NAME)))
             ((symbol-function 'display-buffer)
              (lambda (BUFFER-OR-NAME &rest args)
                (get-buffer-window (switch-to-buffer BUFFER-OR-NAME)))))
     ,@BODY))

(defmacro eriks/unset-key (keymap key)
  "Completely removes KEY from KEYMAP. Just binding it to nil still
leaves the key definition in the map and interferes with lookups in
parent keymaps.
https://www.gnu.org/software/emacs/manual/html_node/elisp/Format-of-Keymaps.html"
  `(setq ,keymap
         (assq-delete-all (string-to-char (kbd ,key))
                          ,keymap)))
