(use-package dired
  :custom
  (dired-listing-switches "-l --almost-all --human-readable --sort=version")
  (shell-command-guess-open "rifle")
  (dired-mouse-drag-files t)
  (dired-auto-revert-buffer t)
  :config
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'wdired-mode 'normal)
  (put 'dired-find-alternate-file 'disabled nil)

  (eriks/universal-argument 'dired-mode-map)

  (defun eriks/dired-mark-up (arg)
    "Mark and go up"
    (interactive "p")
    (save-excursion
      (dired-mark arg))
    (dired-previous-line nil))

  (defun eriks/dired-unmark-up (arg)
    "Unmark and go up"
    (interactive "p")
    (save-excursion
      (dired-unmark arg))
    (dired-previous-line nil))

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

  :gfhook 'auto-revert-mode)

(use-package dirvish
  :ensure t
  :custom
  (dirvish-attributes '(vc-state file-size))
  (dirvish-subtree-always-show-state nil)
  (dirvish-subtree-state-style 'plus)
  (dirvish-use-mode-line 'global)
  (dirvish-use-header-line 'global)
  (dirvish-mode-line-bar-image-width 0)
  (dirvish-header-line-format '(:left (path) :right (wdired)))
  (dirvish-mode-line-format '(:left
                              (sort file-modes " " file-link-number " " file-user ":" file-group " " file-time " " symlink)
                              :right
                              (yank free-space index)))
  (dirvish-side-mode-line-format '(:left (sort " " symlink) :right (yank)))
  (dirvish-side-attributes '(vc-state))
  (dirvish-path-separators '(" ~" " " "/"))
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("m" "/mnt/" "mnt")
     ("r" "/" "root")
     ("t" "/tmp/" "tmp")))
  (dirvish-rsync-args '("--archive" "--verbose" "--human-readable" "--info=progress2"))
  :config
  (require 'dirvish-history)
  (require 'dirvish-ls)
  (require 'dirvish-narrow)
  (require 'dirvish-quick-access)
  (require 'dirvish-rsync)
  (require 'dirvish-side)
  (require 'dirvish-subtree)
  (require 'dirvish-vc)
  (require 'dirvish-yank)
  (require 'dirvish-extras)
  (require 'dirvish-fd)

  (dirvish-override-dired-mode 1)
  (dirvish-side-follow-mode 1)

  ;;TODO: dölj dirvish buffers i C-x b

  (defun eriks/dirvish-up ()
    "Combine subtree up and dired up"
    (interactive)
    (if (dirvish-subtree--parent)
        (dirvish-subtree-up)
      (dired-up-directory)))

  ;;TODO: I would like to unemojify, but they are used in checks all over the place in
  ;;dirvish, so it's not easy to remove them without randombly breaking stuff
  ;; (defconst dirvish-fd-bufname "dirvish-fd-%s-%s")
  ;; (advice-add #'dirvish-fd-switches-ml :filter-return
  ;;             (cl-defun eriks/dirvish-unemojify-header (str)
  ;;               (replace-regexp-in-string "^.*?:" "" str)))

  (defun eriks/dirvish-fd ()
    "The menu only makes sense if `dirvish-fd' is active, so this function
activates the menu if currently searching, otherwise starts a search."
    (interactive)
    (let ((prefix (car (split-string dirvish-fd-bufname "%s"))))
      (if (string-prefix-p prefix (buffer-name))
          (call-interactively 'dirvish-fd-switches-menu)
        (call-interactively 'dirvish-fd))))

  ;; make pp be paste
  (->> dirvish-yank-keys
       (cl-map 'list
               (lambda (ele) (if (eq 'dirvish-yank (nth 2 ele))
                                 (cons "p" (cdr ele))
                               ele)))
       (setopt dirvish-yank-keys))

  (define-advice dirvish-wdired-enter-a (:after (&rest rest) evil)
    "Dirvish overrides the cursor as another type..."
    (kill-local-variable 'evil-normal-state-cursor)
    (evil-refresh-cursor))

  (dirvish-define-mode-line wdired
    "A `wdired-mode' indicator."
    (when (eq major-mode 'wdired-mode)
      (propertize "Editable Dired" 'face 'font-lock-negation-char-face)))

  (define-advice dirvish--vc-root-dir (:override nil projectile)
    "Use projectile instead of `vc-root-dir'"
    (expand-file-name
     (or (projectile-project-root) default-directory)))

  (define-advice dirvish-open-file (:around (org dv find-fn file) frames-only)
    "Don't run the session cleanup code if just opening a file normally, let
`frames-only-mode' create a new frame for the file and let the session
live. Use `dired-find-alternate-file' to also kill the current session."
    (if (eq find-fn 'find-file)
        (funcall find-fn file)
      (funcall org dv find-fn file)))

  :general
  ('global
   "C-x d" 'dirvish
   "C-x C-d" 'dirvish-side)
  ('dirvish-mode-map
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "J" 'dired-next-dirline
   "K" 'dired-prev-dirline
   "f" 'dired-goto-file
   "l" 'dired-find-file ;;TODO: subtree toggle om i dirvish-side och på en mapp?
   "h" 'eriks/dirvish-up

   "C-d" 'eriks/scroll-up-half
   "C-u" 'eriks/scroll-down-half
   "i" 'eriks/scroll-up-half-other-window
   "I" 'eriks/scroll-down-half-other-window
   "C-f" 'scroll-up-command
   "C-b" 'scroll-down-command
   "g" 'beginning-of-buffer
   "G" 'end-of-buffer

   "zf" 'dirvish-layout-toggle
   "zv" 'dirvish-vc-menu
   "zs" 'dirvish-setup-menu
   "zl" 'dirvish-ls-switches-menu
   "zg" 'dirvish-epa-dired-menu
   "zd" 'dirvish-subdir-menu
   "zr" 'dirvish-renaming-menu
   "zR" 'dirvish-rsync-switches-menu
   "y" 'dirvish-file-info-menu
   "o" 'dirvish-quicksort
   "?" 'dirvish-dispatch
   "c" 'dirvish-chxxx-menu
   "*" 'dirvish-mark-menu

   "p" 'dirvish-yank-menu
   "v" 'dired-toggle-marks
   "V" 'dired-unmark-all-marks
   "SPC" 'dired-mark
   "M" 'eriks/dired-mark-up
   "U" 'eriks/dired-unmark-up

   "C-o" 'dirvish-history-go-back ;;TODO: difference with dirvish-history-last?
   "C-i" 'dirvish-history-go-forward
   "H" 'dirvish-history-jump

   "TAB" 'dirvish-subtree-toggle
   "t" 'dirvish-subtree-menu

   "n" 'dirvish-narrow
   "s" 'dired-do-async-shell-command
   "r" 'revert-buffer
   "F" 'eriks/dirvish-fd
   "'" 'dirvish-quick-access

   "W" 'wdired-change-to-wdired-mode
   "R" 'eriks/dired-rename
   "C" 'dired-do-compress-to)
  :gfhook
  ;;TODO: `dirvish--preview-file-maybe-truncate' enables evil and shows a cursor, remove it
  ;; it should have its own hook or mode. Would be nice to enable scroll lock there too
  ('(dired-mode-hook dirvish-directory-view-mode-hook dirvish-special-preview-mode-hook
                     dirvish-better-directory-view-mode-hook)
   (cl-defun eriks/dirvish-hide-evil-cursor ()
     "Dirvish tries to hide the cursor, but evil overrides it."
     (setq-local evil-emacs-state-cursor '(nil)))))

(use-package eriks-dirvish-better-dired-preview)
