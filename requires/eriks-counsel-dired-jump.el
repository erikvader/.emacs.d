(defun eriks/counsel-dired-jump-from-find ()
  "Switch to `counsel-dired-jump' from `counsel-find-file'."
  (interactive)
  (ivy-quit-and-run
    (counsel-dired-jump ivy-text (ivy-state-directory ivy-last))))

(defvar eriks/counsel-dired-jump-map (make-sparse-keymap)
  "Map for `counsel-dired-jump'")

(add-to-list 'ivy-hooks-alist
             (cons 'counsel-dired-jump
                   (cl-defun eriks/counsel-dired-jump-keymap-hook ()
                     "`counsel-dired-jump' is using `counsel-find-file-map' instead of its own
for some reason, so this hook makes sure `eriks/counsel-dired-jump-map'
is active during this command."
                     (set-transient-map eriks/counsel-dired-jump-map
                                        (lambda ()
                                          (and current-minibuffer-command
                                               (eq 'counsel-dired-jump
                                                   (ivy-state-caller ivy-last))))))))

(general-def 'counsel-find-file-map
  "´" 'eriks/counsel-dired-jump-from-find)

(general-def 'eriks/counsel-dired-jump-map
  "´" 'counsel-find-file-from-jump
  "`" 'counsel-file-jump-from-find)

(general-def 'counsel-file-jump-map
  "´" 'eriks/counsel-dired-jump-from-find)

(provide 'eriks-counsel-dired-jump)
