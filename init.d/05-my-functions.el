(defun quit-window-kill (&optional not-kill window)
  "Same as `quit-window' except it's kill-argument has opposite meaning."
  (interactive "P")
  (quit-window (not not-kill) window))

(defmacro popup-frame (&rest body)
  "Runs BODY while displaying all new buffers in new separate frames
instead."
  `(let ((display-buffer-overriding-action '(display-buffer-pop-up-frame
                                             ;; emacsclient gives error if no frames are open, saying that it doesn't
                                             ;; know if a GUI frame or a terminal frame should be spawned.
                                             ;; This forces it to use GUI frames in all cases (or something like that)
                                             (pop-up-frame-parameters . ((window-system . x))))))
     ,@body))

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

(defun eriks/init-warn (&rest warn-args)
  "Runs `warn' with WARN-ARGS and also sends a notification with the
same message if there aren't any frames open. This is a nice way to
display warnings during startup of an Emacs instance started in daemon
mode."
  (apply #'warn warn-args)
  (unless (eq (framep (selected-frame)) 'x)
    (require 'notifications)
    (notifications-notify :title "Emacs Warning" :body (apply #'format warn-args))))

(defmacro eriks/hotfix (package version &rest body)
  "Executes BODY but warns using `eriks/init-warn' if the version of
  PACKAGE is not VERSION."
  `(progn
     (unless (equal (pkg-info-package-version ,package) ,version)
       (eriks/init-warn "\"%s\" got updated and hotfix maybe no longer applies" ,package))
     ,@body))
