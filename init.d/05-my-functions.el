(defun quit-window-kill (&optional not-kill window)
  "Same as `quit-window' except it's kill-argument has opposite meaning."
  (interactive "P")
  (quit-window (not not-kill) window))

(defun eriks/display-buffer-same-window (buffer alist)
  "A rule thingy for `display-buffer'. Run
`display-buffer-same-window' but also mark the opened window to
have spawned the frame by itself. This makes it play nice with
`frames-only-mode' because spawned frames will close themselves
when `quit-window' is run.

See `same-buffer'."
  (let ((w (display-buffer-same-window buffer alist)))
    (when (windowp w)
      (display-buffer-record-window 'frame w buffer)
      (set-window-prev-buffers w nil))
    w))

(defmacro same-buffer (&rest body)
  "Run BODY with `display-buffer-overriding-action' set to
  `eriks/display-same-window'. In other words, this will run BODY and
  open all new buffers in the same window. The purpose of this macro
  is to have emacsclient play nicely with `frames-only-mode'.

Example:
  emacsclient -n -c -e '(same-buffer (find-file \"asd\"))'
This will open a new frame visiting \"asd\". When the buffer is
`quit-window':ed the spawned frame will close itself.

One could also use `display-buffer-pop-up-frame' instead, but that
will not allow for prompts like the too-large-file warning to work and
it will not allow for `man' to know how wide it's window is."
  `(let ((display-buffer-overriding-action '(eriks/display-buffer-same-window
                                             (inhibit-same-window . nil))))
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
  ;; TODO: maybe `map-keymap-internal' and `listify-key-sequence' can
  ;; be helpful to improve this? Basically run a filter on all keys and
  ;; only keep those that we want to keep.
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

(defmacro eriks/hotfix (package &rest body)
  "Executes BODY but warns using `eriks/init-warn' if the version of
  PACKAGE is not VERSION."
  `(progn
     (when (epl-package-outdated-p ,package)
       (eriks/init-warn "\"%s\" got updated and hotfix maybe no longer applies" ,package))
     ,@body))

;TODO: use sexp before point instead
(defun eriks/eval-replace ()
  "Evaluates the sexp at point and replaces it with the result."
  (interactive)
  (let ((b (bounds-of-thing-at-point 'sexp))
        (buf (current-buffer)))
    (when (and b buf)
      (goto-char (car b))
      (princ (eval (read buf)) buf)
      (kill-region (car b) (cdr b)))))
