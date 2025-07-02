;;TODO: experiment with `sp-restrict-to-object-interactive' with `sp-prefix-pair-object'
;;to only navigate using parens and not also symbols. Test in a C-like language
(use-package smartparens
  :ensure t
  :diminish
  :init
  (defconst eriks/leader-sp-infix "s" "Infix for smartparens")
  :config
  (require 'smartparens-config)

  (defun eriks/create--newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indentation."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (defun eriks/sp-open-on (paren modes)
    "Makes all delimeter openers in paren \"open\" after enter has been pressed."
    (when (stringp paren)
      (setq paren (list paren)))
    (dolist (i paren)
      (sp-local-pair modes i nil :post-handlers '((eriks/create--newline-and-enter-sexp "RET")
                                                  (eriks/create--newline-and-enter-sexp "<return>")))))

  ;;NOTE: remove normal state binding to let the motion state ones through
  (general-unbind 'normal "[" "]" "<" ">")

  (eriks/defkey-repeat sp-slurp-barf
    :states 'normal
    :keymaps 'smartparens-mode-map
    :prefix eriks/leader
    "]" 'sp-forward-slurp-sexp
    "[" 'sp-backward-slurp-sexp
    "}" 'sp-forward-barf-sexp
    "{" 'sp-backward-barf-sexp)

  (eriks/defkey-repeat sp-transpose
    :states 'normal
    :keymaps 'smartparens-mode-map
    :prefix eriks/leader
    :infix eriks/leader-sp-infix
    "t" 'sp-transpose-sexp)

  (define-advice sp--indent-region (:around (org start end &rest rest) eriks/sp-hack)
    "Ugly advice to make `eriks/defun-sp-wrap' work. This just makes the
function return the region start and end instead of nothing. This will
make `sp-wrap-with-pair' return this value."
    (apply org start end rest)
    (cons start end))

  (cl-defun eriks/sp-wrap-with (open &optional target-pos)
    "Like `sp-wrap-with-pair', with the addition of control where the cursor ends up."
    ;;NOTE: based on the lambda from the :wrap argument to `sp-pair'
    ;;NOTE: it's bananas that `sp-last-wrapped-region' is not set by this... I have to
    ;;calculate it by hand, assume that `sp-wrap-with-pair' puts the point before the
    ;;opening paren, and an ugly advice needs to be added. Would be nice to be able to
    ;;just use `sp-get' and all the goodies in that...
    (let* (;;NOTE: `sp-wrap-with-pair' looks at `sp-pair-list', so convenient functions
           ;;like `sp-get-pair' can't be used
           (close (-> open
                      (cl-find sp-pair-list :key #'car :test #'equal)
                      (or (error "Pair not found '%s'" open))
                      cdr))
           (reg (sp-wrap-with-pair open))
           (open-len (length open))
           (close-len (length close))
           (inner-beg (+ open-len (car reg)))
           (inner-end (+ open-len (cdr reg)))
           (outer-beg (car reg))
           (outer-end (+ open-len close-len (cdr reg))))
      ;;NOTE: visual state messes with the `goto-char' for some reason, so exit it early
      (evil-exit-visual-state)
      (goto-char (or (plist-get (list :after-closing outer-end
                                      :before-closing inner-end
                                      :after-opening inner-beg
                                      :before-opening outer-beg)
                                target-pos)
                     (error "Invalid TARGET-POS '%s'" target-pos)))))

  ;;TODO: how to surround with a tag, XML style?
  ;;TODO: add a buffer local variable for shortcuts. An alist mapping shorter string to
  ;;the opening to wrap with. Maybe support the value being a lambda and delegate to that?
  ;;Could be used for XML tags?
  (defun eriks/sp-surround ()
    "Surround the region or `sp-select-next-thing' with a pair.

Characters are read until a match to an opening or closing delimiter in
`sp-pair-list' is found, then surround with that pair. If the closing
delimiter is pressed, then place the point at it when inserted, else
place it around the opening delimiter."
    (interactive "*")
    (let (query char (running t) (candidates sp-pair-list) fullmatches)
      (while (and running
                  (setq char (read-char-exclusive)))
        (if (= char ?\e)
            (setq running nil)
          (setq query (concat query (list char)))
          (setq candidates (cl-remove-if-not
                            (lambda (pair) (or (string-prefix-p query (car pair))
                                               (string-prefix-p query (cdr pair))))
                            candidates))
          (setq fullmatches (cl-remove-if-not
                             (lambda (pair) (or (equal query (car pair))
                                                (equal query (cdr pair))))
                             candidates))

          (when-let ((open (cond
                            ((and fullmatches
                                  (> (length candidates) 1))
                             (caddr
                              (read-multiple-choice
                               "Ambiguity"
                               (cl-loop for (open . close) in candidates
                                        for c across "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ"
                                        collect (list c (format "%s .. %s" open close) open))
                               nil nil t)))
                            (fullmatches
                             (caar candidates))
                            ((null candidates)
                             (user-error "Could not find pair '%s'" query)))))
            (setq running nil)
            (eriks/sp-wrap-with
             open
             (if (equal query open)
                 :before-opening
               :after-closing)))))))

  ;;TODO: remove? Or use to bind a minor mode specific keybind for long delimiters? Like
  ;;\textbf or something in latex. Or solve that problem instead with some buffer local
  ;;shortcuts in `eriks/sp-surround'?
  (cl-defmacro eriks/defun-sp-wrap (open &optional target-pos)
    "Define a function that will insert the pair OPEN and place the cursor at
TARGET-POS."
    `(cl-defun ,(intern (format "eriks/sp-wrap-%s-%s" open target-pos)) (&optional arg)
       (interactive "*")
       (eriks/sp-wrap-with ,open ,target-pos)))

  (evil-define-text-object eriks/evil-sp-a-sexp (count &optional beg end type)
    "Same as `eriks/evil-sp-inner-sexp', but also includes the delimiters,
prefix and suffix."
    (if-let ((bounds (sp-get-enclosing-sexp)))
        (sp-get bounds
          (evil-range :beg-prf :end-suf))
      (user-error "Not inside an sexp")))

  (evil-define-text-object eriks/evil-sp-inner-sexp (count &optional beg end type)
    "Text object for the inner parts of the enclosing delimiters, according
to smartparens"
    (if-let ((bounds (sp-get-enclosing-sexp)))
        (sp-get bounds
          (evil-range :beg-in :end-in))
      (user-error "Not inside an sexp")))

  (general-def 'inner
    "s" 'eriks/evil-sp-inner-sexp)

  (general-def 'outer
    "s" 'eriks/evil-sp-a-sexp)

  (eriks/leader-def 'normal
    :keymaps 'smartparens-mode-map
    :infix eriks/leader-sp-infix
    "s" 'sp-split-sexp
    "j" 'sp-join-sexp
    "d" 'sp-splice-sexp-killing-around)

  ;;TODO: don't activate auto insert inside quotes?

  (evil-define-command eriks/sp-operator ()
    "A dispatcher to call appropriate smartparens functions from, for
example, d in normal state."
    (setq evil-inhibit-operator t)
    (cond
     ((eq evil-this-operator 'evil-delete)
      (call-interactively 'sp-splice-sexp))
     ((eq evil-this-operator 'evil-change)
      (call-interactively 'sp-rewrap-sexp))
     (t
      (user-error "Invalid operator %s" evil-this-operator))))

  :ghook ('prog-mode-hook '(show-smartparens-mode smartparens-mode))
  :custom
  (sp-navigate-interactive-always-progress-point t)
  (sp-autodelete-closing-pair t)
  (sp-autodelete-opening-pair t)
  (sp-autodelete-pair t)
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert t)
  (sp-highlight-pair-overlay nil)
  (sp-navigate-reindent-after-up nil)
  (sp-navigate-reindent-after-up-in-string nil)
  :general-config
  ('normal
   'smartparens-mode-map
   "s" 'eriks/sp-surround)
  ('operator
   'smartparens-mode-map
   "s" 'eriks/sp-operator)
  ('motion
   'smartparens-mode-map
   ;;TODO: should these be in a repeat map?
   :prefix "g"
   "L" 'sp-next-sexp
   "H" 'sp-previous-sexp
   ">" 'sp-down-sexp
   "<" 'sp-backward-down-sexp)
  ('motion
   'smartparens-mode-map
   "]" 'sp-end-of-next-sexp
   "[" 'sp-beginning-of-previous-sexp
   "L" 'sp-forward-sexp
   "H" 'sp-backward-sexp
   "<" 'sp-backward-up-sexp
   ">" 'sp-up-sexp
   "(" 'sp-beginning-of-sexp
   ")" 'sp-end-of-sexp)
  ('insert
   'smartparens-mode-map
   "C-u" 'sp-up-sexp))
