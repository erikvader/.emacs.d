;; TODO: eat
(use-package eshell
  :custom
  (eshell-prompt-function #'eriks/eshell-prompt)
  (eshell-scroll-show-maximum-output nil)
  (eshell-banner-message "")
  :config
  (define-advice evil-collection-eshell-setup-keys (:before (&rest _args) eriks)
    "Evil collections defines the keys in a hook because eshell is weird or something?"
    (general-def 'normal 'eshell-mode-map
      ;; NOTE: to match comint
      "C-n" 'eshell-next-input
      "C-p" 'eshell-previous-input)

    (general-def 'eshell-mode-map
      "M-." 'eriks/eshell-yank-last-arg)

    (general-def 'motion 'eshell-mode-map
      ;; NOTE: The evil variant tries to preserve the column, which means it doesn't
      ;; always go to the last prompt, but sometime on it. This also makes it difficult to
      ;; catch a command that it outputting a lot of text. The normal emacs variant works
      ;; best in this mode.
      "G" 'end-of-buffer))
  (evil-collection-eshell-setup)
  (evil-set-initial-state 'eshell-mode 'normal)

  ;; NOTE: using the mode doesn't work for some reason
  (add-to-list 'popper-reference-buffers (eriks/regexp-quote-all eshell-buffer-name))

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

  (defun eriks/eshell-prompt ()
    "A prompt"
    ;; NOTE: must be compatible with `eshell-prompt-regexp'
    (let ((cwd (-> (eshell/pwd)
                   (abbreviate-file-name)
                   (fish-path)
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
  :gfhook #'reset-scroll-margin)

(use-package term
  :config
  (eriks/leader-def 'normal
    :infix "o"
    "a" 'ansi-term))

(use-package shell
  :config
  (add-to-list 'popper-reference-buffers 'shell-mode)
  (eriks/leader-def 'normal
    :infix "o"
    "s" 'shell))

(use-package ielm
  :custom
  (ielm-header "")
  :config
  (add-to-list 'popper-reference-buffers 'inferior-emacs-lisp-mode)
  (eriks/leader-def 'normal
    :infix "o"
    "i" 'ielm))

(use-package comint
  :custom
  (comint-scroll-show-maximum-output nil)
  :config
  (evil-set-initial-state 'comint-mode 'normal)
  (evil-collection-comint-setup)
  :general-config
  ('insert
   'comint-mode-map
   "<up>" 'comint-previous-matching-input-from-input
   "<down>" 'comint-next-matching-input-from-input)
  ('comint-mode-map
   "M-p" 'comint-previous-matching-input-from-input
   "M-n" 'comint-next-matching-input-from-input)
  :gfhook #'reset-scroll-margin)
