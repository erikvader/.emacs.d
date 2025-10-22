(use-package dired
  :custom
  ;; NOTE: filenames with spaces and other weird characters is not handled well, but
  ;; adding any kind of quoting style will make the filenames uglier in the normal case,
  ;; so hope no weird filenames are encountered!
  (dired-listing-switches "-l --almost-all --human-readable --sort=version --time-style=long-iso")
  (shell-command-guess-open "rifle")
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
         (when (eq major-mode 'dired-mode)
           (format "Kill dired buffer '%s'? " (buffer-name buffer)))))
     #'kill-buffer
     (buffer-list)))
  :gfhook
  'auto-revert-mode
  'dired-hide-details-mode
  :general-config
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
  (dired-preview-display-action-alist (cl-defun eriks/dired-preview-right ()
                                        ;; NOTE: copy of
                                        ;; `dired-preview-display-action-alist-below', but
                                        ;; always puts it on the right instead of below
                                        (let ((width (floor (window-total-width) 2)))
                                          `((display-buffer-in-direction)
                                            (direction . right)
                                            (window-width . ,width)
                                            (dedicated . t))))))

(use-package dired-ranger
  :ensure t
  :custom
  (dired-ranger-bookmark-reopen 'always)
  (dired-ranger-bookmarks '((?h "~" . t)
                            (?r "/" . t)
                            (?t "/tmp" . t)
                            (?w "~/Downloads" . t)
                            (?d "~/Documents" . t)
                            (?D "~/dotfiles" . t)))
  :config
  (defun eriks/dired-rename ()
    "Rename the selected file, like cw in ranger. Basically
`dired-do-rename', but without the code for handling marks."
    (interactive nil dired-mode)
    (let* ((cur (dired-get-filename))
           (dir (file-name-directory cur))
           new)
      (when (or (string-equal cur dir)
                (not (file-name-absolute-p cur)))
        (error "Invalid path to rename: %s" cur))
      (setq new (read-string "Rename: " (file-name-nondirectory cur)))
      (unless (and (stringp new)
                   (not (string-empty-p new))
                   (= 1 (length (file-name-split new))))
        (user-error "Invalid filename: %s" new))
      (dired-create-files #'dired-rename-file
                          "Rename"
                          (list cur)
                          (lambda (old) (file-name-concat dir new))
                          dired-keep-marker-rename)))

  (define-advice dired-ranger-bookmark-visit (:around (org &rest args) kill-when-opening-new)
    "Makes this function respect `dired-kill-when-opening-new-dired-buffer'."
    (if dired-kill-when-opening-new-dired-buffer
        (cl-letf* (((symbol-function 'find-file) (symbol-function 'find-alternate-file)))
          (apply org args))
      (apply org args)))

  (defun eriks/dired-ranger-copy-append (arg)
    "Append the currently selected files to the current fileset."
    (interactive "P")
    (dired-ranger-copy (not arg)))
  :general-config
  ('normal
   'dired-mode-map
   :prefix "r"
   "p" 'dired-ranger-paste
   "m" 'dired-ranger-move
   "c" 'dired-ranger-copy
   "a" 'eriks/dired-ranger-copy-append
   "r" 'eriks/dired-rename
   "m" 'dired-ranger-bookmark
   "'" 'dired-ranger-bookmark-visit))

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
