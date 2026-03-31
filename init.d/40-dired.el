;; TODO: Figure out if i want to use `diredp-move-files-named-in-kill-ring' and
;; `diredp-yank-files' to move and copy files in dired. Found in dired+.
(use-package dired
  :custom
  ;; NOTE: filenames with spaces and other weird characters is not handled well, but
  ;; adding any kind of quoting style will make the filenames uglier in the normal case,
  ;; so hope no weird filenames are encountered!
  (dired-listing-switches "-l --almost-all --human-readable --sort=version --time-style=long-iso")
  (shell-command-guess-functions '(shell-command-guess-open
                                   eriks/shell-command-guess-rifle
                                   shell-command-guess-dired-optional
                                   shell-command-guess-mailcap
                                   shell-command-guess-xdg
                                   shell-command-guess-dired-default
                                   shell-command-guess-dired-user))
  (dired-dwim-target t)
  (dired-free-space nil)
  (dired-mouse-drag-files t)
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-collection-dired-setup)
  (evil-collection-wdired-setup)
  (put 'dired-find-alternate-file 'disabled nil)

  (defun eriks/kill-all-dired-buffers ()
    "Prompts to kill every buffer that is visiting a dired buffer."
    (interactive)
    (map-y-or-n-p
     (lambda (buffer)
       (with-current-buffer buffer
         (when (derived-mode-p 'dired-mode)
           (format "Kill dired buffer '%s'? " (buffer-name buffer)))))
     #'kill-buffer
     (buffer-list)))

  (defun eriks/shell-command-guess-rifle (commands _files)
    "Poulate COMMANDS with the rifle command."
    (if (executable-find "rifle")
        (cons "rifle" commands)
      commands))

  (define-advice dired-rename-file (:filter-args (args) fix-newname-dir)
    "Make this function work when NEWNAME is a directory.

The underlying function `rename-file' will move FILE into NEWNAME if it
ends with a slash, so that will work. But `set-visited-file-name' is
called afterwards, and that errors if NEWNAME is a directory. There are
no checks for this in `dired-rename-file', so this advice makes sure
NEWNAME is not a directory by following the same logic as
`rename-file'and `rename-visited-file'. It is possible that dired never
calls this function with NEWNAME as a directory, but
`counsel-find-file-move' does. It was actually an advice from
smart-mode-line that caused problems, but the normal `set-visited-file'
should fail as well."
    (cl-destructuring-bind (file newname &rest rest) args
      (cl-list* file
                (if (directory-name-p newname)
                    (file-name-concat newname (file-name-nondirectory file))
                  newname)
                rest)))

  :gfhook
  'auto-revert-mode
  'dired-hide-details-mode
  :general-config
  ('global
   "C-x C-d" 'find-name-dired)
  ('dired-jump-map
   ;;NOTE: can't go down after a jump with this
   "j" nil)
  ('normal
   'dired-mode-map
   "gR" 'dired-do-redisplay
   "gm" 'evil-set-marker
   "sg" 'dired-goto-subdir
   "sk" 'dired-kill-subdir
   "sh" 'dired-hide-subdir
   "sH" 'dired-hide-all
   "<" 'dired-prev-subdir
   ">" 'dired-next-subdir
   ;; NOTE: don't override my `repeat'
   "-" nil
   ;; NOTE: don't override my `counsel-rg'
   "M-s" nil))

(use-package dired-preview
  :ensure t
  :general-config
  ('normal
   'dired-mode-map
   :prefix "g"
   "p" 'dired-preview-global-mode)
  :custom
  (dired-preview-max-size (* 10 1024 1024))
  ;; TODO: do i really want this?
  ;; (dired-preview-display-action-alist (cl-defun eriks/dired-preview-right ()
  ;;                                       ;; NOTE: copy of
  ;;                                       ;; `dired-preview-display-action-alist-below', but
  ;;                                       ;; always puts it on the right instead of below
  ;;                                       (let ((width (floor (window-total-width) 2)))
  ;;                                         `((display-buffer-in-direction)
  ;;                                           (direction . right)
  ;;                                           (window-width . ,width)
  ;;                                           (dedicated . t)))))
  )

(use-package dired-collapse
  :ensure t
  :config
  (define-globalized-minor-mode dired-collapse-global-mode
    dired-collapse-mode
    (lambda () (dired-collapse-mode 1))
    :predicate '(dired-mode))
  :general-config
  ('normal
   'dired-mode-map
   :prefix "g"
   "c" 'dired-collapse-global-mode))

(use-package dired-rainbow
  :ensure t
  :custom
  ;; NOTE: matches --time-style=long-iso
  (dired-hacks-datetime-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}")
  :config
  ;; NOTE: I could have used `eshell-ls-use-in-dired' instead to get the colors, I think,
  ;; but the output of it is not very flexible, so I can't make it look like I want.
  (dired-rainbow-define-chmod executable eshell-ls-executable "-.*x.*"))
