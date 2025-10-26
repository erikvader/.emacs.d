;; color theme
(defface todo-face nil "face for TODO")
(defface fixme-face nil "face for FIXME")
(defface note-face nil "face for NOTE")
(defface tab-face nil "face for tabs")

(define-advice color-rgb-to-hex  (:filter-args (args) change-default)
  "This function will, by default, return RGB strings with four characters
per component instead of usual two. Some code is not expecting that,
like whatever is setting `ivy-minibuffer-match-face-1', it truncates the
string and produces a completely different color. This advice changes
the default number back to the usual 2."
  (cl-destructuring-bind (red green blue &optional digits-per-component) args
    (list red green blue (or digits-per-component 2))))

(use-package dracula-theme
  :ensure t
  :config
  (define-advice custom-theme-set-faces (:filter-args (args) dracula-black-background)
    "An advice to map some colors to others, noteably the default background color."
    (cl-block nil
      (cl-destructuring-bind (theme &rest specs) args
        (unless (eq theme 'dracula)
          (cl-return args))

        (let ((display-graphical '((min-colors 16777216)))
              (dracula-bg "#282a36")
              (dracula-comment "#6272a4")
              (bg2 "#373844")
              (bg3 "#565761")
              (fg2 "#e2e2dc")
              (fg3 "#ccccc7")
              (dracula-orange "#ffb86c")
              (dracula-purple "#bd93f9")
              (dracula-yellow "#f1fa8c")
              (dracula-red "#ff5555")
              (dracula-pink "#ff79c6")
              (dracula-green "#50fa7b")
              (dracula-cyan "#8be9fd")
              (dracula-current "#44475a")
              (dracula-fg "#f8f8f2"))
          (cl-labels ((ovwr (xs face &rest spec)
                        (cons `(,face ((,display-graphical ,spec))) xs))
                      (modify (xs face &rest kwargs)
                        (cl-loop for (key value) on kwargs by #'cddr
                                 do (let ((face-plist (--> xs
                                                           ;;NOTE: these `car's can't be `setf' if
                                                           ;;nil, so face must exist with
                                                           ;;a display-graphical for this to work.
                                                           (alist-get face it)
                                                           (car it)
                                                           (alist-get display-graphical it nil nil #'equal))))
                                      (setf (cl-getf (car face-plist) key)
                                            value))
                                 finally return xs))
                      (rm (xs face)
                        (setf (alist-get face xs t t) t)
                        xs)
                      (subs (xs)
                        ;; NOTE: this does not support dotted lists longer than two
                        (cond ((and (consp xs) (atom (cdr xs)) (cons (subs (car xs)) (subs (cdr xs)))))
                              ((listp xs) (mapcar #'subs xs))
                              ;; background. Make it as dark as possible
                              ((equal xs dracula-bg) "black")
                              ;; darken alt backgrounds to match new background
                              ((member xs (list bg2 bg3 dracula-current)) (color-darken-name xs 50))
                              ;; font-lock-comment. Contrast better with the new background
                              ((equal xs dracula-comment) (color-lighten-name xs 25))
                              (t xs))))
            (cl-list* theme
                      (-> specs
                          (modify 'mode-line-inactive :background dracula-current :box dracula-current)
                          (ovwr 'evil-quickscope-first-face :foreground dracula-green :underline t :weight 'bold)
                          (ovwr 'evil-quickscope-second-face :foreground dracula-yellow :underline t :weight 'bold)
                          (ovwr 'Man-overstrike :weight 'bold :foreground dracula-pink)
                          (ovwr 'Man-underline :weight 'bold :foreground dracula-purple)
                          (ovwr 'todo-face :foreground "orange1" :weight 'bold)
                          (ovwr 'fixme-face :inherit 'todo-face)
                          (ovwr 'note-face :foreground "chartreuse1" :weight 'bold)
                          (ovwr 'eshell-prompt)
                          (ovwr 'tab-face :strike-through t :foreground "salmon")
                          (ovwr 'vertical-border :foreground dracula-fg)
                          (ovwr 'region :inverse-video t)
                          (ovwr 'sml/filename :inherit 'sml/global :foreground "gold2" :weight 'bold)
                          (ovwr 'sml/prefix :inherit 'sml/global :foreground "chocolate1")
                          (ovwr 'sml/read-only :inherit 'sml/not-modified :foreground "DeepSkyBlue")
                          (ovwr 'aw-mode-line-face :foreground "violet" :weight 'bold)
                          (ovwr 'vline :inherit 'hl-line)
                          (ovwr 'dired-directory :foreground dracula-purple :weight 'bold)
                          (ovwr 'eshell-ls-directory :inherit 'dired-directory)
                          (ovwr 'ivy-subdir :inherit 'dired-directory)
                          (ovwr 'counsel-evil-register-face :foreground dracula-pink)
                          (ovwr 'dired-symlink :foreground dracula-cyan :weight 'bold)
                          (ovwr 'eshell-ls-symlink :inherit 'dired-symlink)
                          (ovwr 'eshell-ls-executable :foreground dracula-green :weight 'bold)
                          (ovwr 'dired-broken-symlink :foreground dracula-red :weight 'bold)
                          (ovwr 'eshell-ls-missing :inherit 'dired-broken-symlink)
                          (ovwr 'eshell-ls-product :inherit 'dired-ignored)
                          (ovwr 'eshell-ls-clutter :inherit 'dired-ignored)
                          (ovwr 'eshell-ls-backup :inherit 'dired-ignored)
                          (ovwr 'dired-special :foreground dracula-yellow :weight 'bold)
                          (ovwr 'eshell-ls-special :inherit 'dired-special)
                          (ovwr 'dired-set-id :background dracula-yellow :foreground dracula-bg)
                          (ovwr 'swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
                          (ovwr 'swiper-match-face-2 :background dracula-green :foreground dracula-bg)
                          (ovwr 'swiper-match-face-3 :background dracula-yellow :foreground dracula-bg)
                          (ovwr 'swiper-match-face-4 :background dracula-cyan :foreground dracula-bg)
                          (ovwr 'ivy-minibuffer-match-face-2 :box `(:line-width (-1 . -1) :color ,dracula-green))
                          (ovwr 'ivy-minibuffer-match-face-3 :box `(:line-width (-1 . -1) :color ,dracula-yellow))
                          (ovwr 'ivy-minibuffer-match-face-4 :box `(:line-width (-1 . -1) :color ,dracula-cyan))
                          (modify 'magit-diff-hunk-heading-highlight :background (color-lighten-name dracula-current 40))
                          (ovwr 'diff-hunk-header :inherit 'magit-diff-hunk-heading)
                          (ovwr 'diff-header :inherit 'magit-diff-file-heading)
                          (ovwr 'diff-file-header :inherit 'magit-diff-file-heading-highlight)
                          (ovwr 'diff-hl-change :foreground dracula-yellow :background dracula-yellow)
                          (ovwr 'diff-indicator-changed :foreground dracula-yellow)
                          (ovwr 'diff-refine-added :background (color-darken-name dracula-green 65) :foreground dracula-fg)
                          (ovwr 'diff-refine-removed :background (color-darken-name dracula-red 65) :foreground dracula-fg)
                          (ovwr 'diff-added :inherit 'magit-diff-added)
                          (ovwr 'diff-removed :inherit 'magit-diff-removed)
                          subs
                          (ovwr 'lazy-highlight :background dracula-comment :foreground dracula-fg))))))))

  ;;NOTE: Some color manipulation functions, like `color-lighten-name', depend on the type
  ;;of the frame and its capabilities. The function `color-values' is the one responsible
  ;;for this behaviour. I only care about GUI frames, so only load this theme after one is
  ;;created. The colors can get incorrect if this is evaluated as a daemon without a
  ;;frame.
  (general-after-gui
    (load-theme 'dracula t nil)))

;; Easier to remember aliases
(defalias 'theme-enable #'enable-theme)
(defalias 'theme-disable #'disable-theme)

;; font
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 14"))
(set-fontset-font t 'symbol "Noto Color Emoji")

;; transparent background
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; column line
(add-hook 'eriks/editable-file-hook
          (cl-defun eriks/activate-display-fill-column ()
            (setq display-fill-column-indicator t)
            (setq display-fill-column-indicator-character ?│)))

;; font-locks
(defun eriks/add-marker-font-locks ()
  "Makes FIXME, TODO and NOTE get highlighted in current buffer"
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\):" 1 'fixme-face t)
     ("\\<\\(TODO\\):" 1 'todo-face t)
     ("\\<\\(NOTE\\):" 1 'note-face t))))

(add-hook 'eriks/editable-file-hook #'eriks/add-marker-font-locks)

(defun eriks/add-tab-font-lock ()
  "Display tabs in the current buffer using `tab-face'."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\t" 0 'tab-face t))))

(add-hook 'eriks/editable-file-hook #'eriks/add-tab-font-lock)

(defun eriks/prog-mode-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'eriks/editable-file-hook #'eriks/prog-mode-show-trailing-whitespace)
