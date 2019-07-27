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
(https://www.gnu.org/software/emacs/manual/html_node/elisp/Format-of-Keymaps.html)

This can handle key sequences like \"C-k\", \"C-k g d M-e\" and
\"<delete>\", but not (atleast at the moment) \"[remap fun]\",
state bindings from evil and other things that can't be compared with `eq'.

Key sequences with meta (\"M-a\") has to be rewritten as \"<ESC> a\"
because they are stored like that in keymaps."
  `(cl-labels ((remove-key (map keys)
                           (cond ((and (null keys)
                                       (not (keymapp map)))
                                  t)
                                 ((or (null keys)
                                      (null map))
                                  nil)
                                 (t
                                  (let ((removed (remove-key (cdr (assq (car keys) map))
                                                             (cdr keys))))
                                    (cond ((or (eq removed t)
                                               (equal removed '(keymap)))
                                           (assq-delete-all (car keys) map))
                                          ((keymapp removed)
                                           (setf (cdr (assq (car keys) map)) removed)
                                           map)
                                          (t
                                           nil)))))))
     (let ((new (remove-key ,keymap (listify-key-sequence (kbd ,key)))))
       (when (keymapp new)
         (setq ,keymap new)))))

(defun eriks/add-to-lists (list items)
  "Same as `add-to-list' except that ITEMS is a list of elements to be
added."
  (mapc (lambda (i)
          (add-to-list list i))
        items))
