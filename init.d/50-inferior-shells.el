;; TODO: eat
;; TODO: dircolors
;; TODO: running compile from eshell will make the program (cargo) emit escape codes.
;; Either make it don't or add ansi support to compile
;; TODO: remove and use shell-mode instead?
(use-package eshell
  :custom
  (eshell-prompt-function #'eriks/eshell-prompt)
  (eshell-banner-message "")
  (eshell-hist-ignoredups 'erase)
  (eshell-input-filter 'eshell-input-filter-initial-space)
  (eshell-history-size 10000)
  :config
  (define-advice evil-collection-eshell-setup-keys (:before (&rest _args) eriks)
    "Evil collections defines the keys in a hook because eshell is weird or something?"
    ;; TODO: make these match comint
    (general-def 'normal 'eshell-mode-map
      ;; NOTE: to match comint
      ;; TODO: why both here and in hist map?
      "C-n" 'eshell-next-input
      "C-p" 'eshell-previous-input
      ;; NOTE: make it easier to close the window. Q is bound to an evil extension normally
      "q" 'quit-window
      "Q" 'evil-record-macro)

    (general-def 'eshell-mode-map
      "M-." 'eriks/eshell-yank-last-arg)

    (general-def 'normal 'eshell-hist-mode-map
      ;; TODO: these arrows should only do this in insert?
      "<up>" #'eshell-previous-matching-input-from-input
      "<down>" #'eshell-next-matching-input-from-input
      ;; TODO: is M-n and M-p bound correctly?
      "C-p" #'eshell-previous-input
      "C-n" #'eshell-next-input
      "C-r" #'eshell-previous-matching-input)

    (general-def 'motion 'eshell-mode-map
      ;; NOTE: The evil variant tries to preserve the column, which means it doesn't
      ;; always go to the last prompt, but sometime on it. This also makes it difficult to
      ;; catch a command that it outputting a lot of text. The normal emacs variant works
      ;; best in this mode.
      "G" 'end-of-buffer))
  (evil-collection-eshell-setup)
  (evil-set-initial-state 'eshell-mode 'normal)

  (defun eriks/eshell-yank-last-arg ()
    "Inserts the last argument of the previous command. Can also be referenced using $_"
    (interactive)
    (unless eshell-last-arguments
      (user-error "No previous argument of %s" eshell-last-command-name))
    (-> eshell-last-arguments
        last
        car
        substring-no-properties
        insert))

  (eriks/leader-def 'normal
    :infix "o"
    "e" 'eshell)

  (defun eriks/eshell-handle-ansi-osc ()
    "Handle ANSI OSC codes"
    (require 'ansi-color)
    (let (ansi-osc-handlers)
      (ansi-osc-apply-on-region eshell-last-output-start
                                eshell-last-output-end)))
  (add-to-list 'eshell-output-filter-functions 'eriks/eshell-handle-ansi-osc)

  (defun eriks/eshell-prompt ()
    "A prompt"
    ;; NOTE: must be compatible with `eshell-prompt-regexp'
    (let ((cwd (-> (eshell/pwd)
                   (abbreviate-file-name)
                   (fish-path :lastfull 2 :complen 2)
                   (propertize 'face 'eshell-ls-directory)))
          (status (unless (eshell-exit-success-p)
                    (-> (format " [%d]" eshell-last-command-status)
                        (propertize 'face 'error))))
          (end (-> (if (= (file-user-uid) 0) " #" " $")
                   (propertize 'face '(:weight bold)))))
      (concat cwd status end " ")))

  :general-config
  ('eshell-hist-mode-map
   ;; NOTE: restore my counsel-rg
   "M-s" nil)
  :gfhook
  ('kill-emacs-hook (cl-defun eriks/kill-eshell-on-kill-emacs ()
                      "Kills any eshell buffers when emacs itself is killed to make sure
                      saving history and other things in
                      `eshell-exit-hook' is actually done."
                      (dolist (buf (buffer-list))
                        (with-current-buffer buf
                          (when (derived-mode-p 'eshell-mode)
                            (kill-buffer)))))))

(use-package eshell-up
  :ensure t)

(use-package term
  :config
  (eriks/leader-def 'normal
    :infix "o"
    "a" 'ansi-term))

(use-package comint
  :config
  (evil-set-initial-state 'comint-mode 'insert)
  (evil-collection-comint-setup)

  (defun eriks/comint-scroll-prompt-to-bottom (win)
    "Make sure the prompt is at the bottom on window size changes."
    (when (window-live-p win)
      (with-selected-window win
        (when (and (derived-mode-p 'comint-mode)
                   (eobp))
          (recenter -1)))))
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  ;; TODO: enable savehist-mode and use savehist-length here so they are the same?
  (comint-input-ring-size 10000)
  :general-config
  ('comint-mode-map
   "C-l" 'comint-clear-buffer)
  ('insert
   'comint-mode-map
   ;; Make it more bash/minibuffer-like. NOTE: It didn't work to simply bind to nil
   "C-a" 'move-beginning-of-line ;; I have never used this
   "C-k" 'kill-line ;; not using digraphs
   "C-e" 'move-end-of-line ;; not really useful in this context
   ;; History
   "M-p" 'comint-previous-matching-input-from-input
   "M-n" 'comint-next-matching-input-from-input
   ;; NOTE: C-r is taken by evil
   "M-r" 'comint-history-isearch-backward-regexp
   ;; To send stuff
   "RET" 'comint-send-input)
  ('normal
   'comint-mode-map
   ;; NOTE: make it easier to close the window. Q is bound to an evil extension normally
   "q" 'quit-window
   "Q" 'evil-record-macro
   ;; Do it in normal mode as well to mimic `set -o vi' in bash
   "RET" 'comint-send-input)
  ('motion
   'comint-mode-map
   ;; NOTE: The evil variant tries to preserve the column, which means it doesn't
   ;; always go to the last prompt, but sometime on it. This also makes it difficult to
   ;; catch a command that it outputting a lot of text. The normal emacs variant works
   ;; best in this mode.
   "G" 'end-of-buffer)
  :gfhook
  ('comint-output-filter-functions #'comint-osc-process-output)
  (nil (cl-defun eriks/comint-install-size-change-function ()
         (add-hook 'window-size-change-functions #'eriks/comint-scroll-prompt-to-bottom nil t))))

(use-package shell
  :config
  (eriks/leader-def 'normal
    :infix "o"
    "s" 'shell)
  :custom
  (shell-kill-buffer-on-exit t)
  (shell-font-lock-keywords nil)
  :general-config
  ('insert
   'shell-mode-map
   "M-." 'comint-insert-previous-argument)
  :gfhook
  (nil (cl-defun eriks/shell-mode-hook ()
         (face-remap-add-relative 'comint-highlight-prompt 'default))))

(use-package coterm
  :ensure t
  :config
  (coterm-mode 1)
  :general-config
  ('comint-mode-map
   "C-," #'coterm-char-mode-cycle))

(use-package ielm
  :custom
  (ielm-header "")
  :config
  (eriks/leader-def 'normal
    :infix "o"
    "i" 'ielm)
  :general-config
  ('normal
   'ielm-map
   "RET" 'ielm-return))
