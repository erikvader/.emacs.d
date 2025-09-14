;; color theme
(defface todo-face nil "face for TODO")
(defface fixme-face nil "face for FIXME")
(defface note-face nil "face for NOTE")
(defface tab-face nil "face for tabs")

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
                        (cond ((listp xs) (mapcar #'subs xs))
                              ;; background. Make it as dark as possible
                              ((equal xs dracula-bg) "black")
                              ;; darken alt backgrounds to match new background
                              ((member xs (list bg2 bg3 dracula-current)) (color-darken-name xs 40))
                              ;; font-lock-comment. Contrast better with the new background
                              ((equal xs dracula-comment) (color-lighten-name xs 10))
                              (t xs))))
            (cl-list* theme
                      (-> specs
                          (modify 'mode-line-inactive :background dracula-current :box dracula-current)
                          (ovwr 'evil-quickscope-first-face :foreground dracula-green :underline t :weight 'bold)
                          (ovwr 'evil-quickscope-second-face :foreground dracula-yellow :underline t :weight 'bold)
                          (ovwr 'diff-hl-change :foreground dracula-yellow :background dracula-yellow)
                          (ovwr 'diff-hl-delete :foreground dracula-red :background dracula-red)
                          (ovwr 'diff-hl-insert :foreground dracula-green :background dracula-green)
                          (ovwr 'todo-face :foreground "orange1" :weight 'bold)
                          (ovwr 'fixme-face :inherit 'todo-face)
                          (ovwr 'note-face :foreground "chartreuse1" :weight 'bold)
                          (ovwr 'tab-face :strike-through t :foreground "salmon")
                          (ovwr 'lazy-highlight :background dracula-comment :foreground dracula-fg)
                          (ovwr 'vertical-border :foreground dracula-fg)
                          (ovwr 'region :inverse-video t)
                          (ovwr 'sml/filename :inherit 'sml/global :foreground "gold2" :weight 'bold)
                          (ovwr 'sml/prefix :inherit 'sml/global :foreground "chocolate1")
                          (ovwr 'sml/read-only :inherit 'sml/not-modified :foreground "DeepSkyBlue")
                          (ovwr 'aw-mode-line-face :foreground "violet" :weight 'bold)
                          (ovwr 'vline :inherit 'hl-line)
                          (ovwr 'swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
                          (ovwr 'swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
                          (ovwr 'swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
                          (ovwr 'swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)
                          (modify 'ivy-minibuffer-match-face-4 :background dracula-cyan)
                          (modify 'magit-diff-hunk-heading-highlight :background (color-lighten-name dracula-current 40))
                          (ovwr 'diff-refine-added :background (color-darken-name dracula-green 55) :foreground dracula-fg)
                          (ovwr 'diff-refine-removed :background (color-darken-name dracula-red 55) :foreground dracula-fg)
                          subs)))))))

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
(add-hook 'prog-mode-hook
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

(add-hook 'prog-mode-hook #'eriks/add-marker-font-locks)

(defun eriks/add-tab-font-lock ()
  "Display tabs in the current buffer using `tab-face'."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\t" 0 'tab-face t))))

(add-hook 'prog-mode-hook #'eriks/add-tab-font-lock)
(add-hook 'text-mode-hook #'eriks/add-tab-font-lock)

(defun eriks/prog-mode-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'eriks/prog-mode-show-trailing-whitespace)
