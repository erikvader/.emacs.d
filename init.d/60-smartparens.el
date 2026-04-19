(use-package smartparens
  :ensure t
  :diminish
  :config
  (add-to-list 'sp--html-modes 'mhtml-mode)
  (require 'smartparens-config)

  (sp-with-modes sp--html-modes
    (sp-local-pair "<" nil :actions '(autoskip navigate)))

  (dolist (mode sp--html-modes)
    (when-let ((hook (-> mode
                         symbol-name
                         (concat "-hook")
                         intern-soft)))
      (add-hook hook 'smartparens-mode)))

  (sp-local-pair 'lua-mode "if" nil :actions nil)
  (sp-local-pair 'lua-mode "while" nil :actions nil)
  (sp-local-pair 'lua-mode "for" nil :actions nil)
  (sp-local-pair 'lua-mode "function" nil :actions nil)
  (sp-local-pair 'm4-mode "`" "'" :actions '(insert autoskip navigate))

  ;;NOTE: remove normal state binding to let the motion state ones through
  (general-unbind 'normal "[" "]")

  ;;NOTE: I'm tired of seing the unmatched expression error message
  (cl-callf2 assq-delete-all :unmatched-expression sp-message-alist)

  (eriks/leader-def 'normal 'smartparens-mode-map
    :infix "s"
    ;; TODO: also try/evaluate `sp-indent-adjust-sexp' and `sp-push-hybrid-sexp'
    "h" 'sp-transpose-hybrid-sexp
    "k" 'sp-kill-hybrid-sexp
    "t" 'sp-transpose-sexp
    "w" 'sp-swap-enclosing-sexp
    "s" 'sp-split-sexp
    "j" 'sp-join-sexp
    "d" 'sp-splice-sexp-killing-around)

  (define-advice sp--indent-region (:around (fun start end &optional column) less-aggressive)
    "Don't indent if the operation only touched a single line.

It's really annoying that the current line is shifted to weird places if
the current major modes doesn't like how the current line is indented.
But it's extremely useful when it spans multiple lines, like when
removing an enclosing xml-tag.

See also: `sp-no-reindent-after-kill-modes' and
`sp-no-reindent-after-kill-indent-line-functions'."
    (when (/= (line-number-at-pos start)
              (line-number-at-pos end))
      (funcall fun start end column)))

  :ghook ('(prog-mode-hook conf-mode-hook TeXemode-hook)
          '(show-smartparens-mode smartparens-mode))
  :custom
  (sp-use-subword t)
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
   :prefix ">"
   ">" 'evil-shift-right
   "}" 'sp-slurp-hybrid-sexp
   ")" 'sp-forward-slurp-sexp
   "(" 'sp-backward-barf-sexp)
  ('normal
   'smartparens-mode-map
   :prefix "<"
   "<" 'evil-shift-left
   "}" 'sp-dedent-adjust-sexp
   "(" 'sp-backward-slurp-sexp
   ")" 'sp-forward-barf-sexp)
  ('motion
   'smartparens-mode-map
   :prefix "]"
   "[" 'sp-next-sexp
   "]" 'sp-forward-sexp
   ">" (sp-restrict-to-object-interactive #'sp-prefix-pair-object 'sp-forward-sexp)
   "<" (sp-restrict-to-object-interactive #'sp-prefix-pair-object 'sp-next-sexp)
   ")" 'sp-end-of-next-sexp
   "(" 'sp-beginning-of-next-sexp
   "j" 'sp-down-sexp
   "k" 'sp-up-sexp)
  ('motion
   'smartparens-mode-map
   :prefix "["
   "]" 'sp-previous-sexp
   "[" 'sp-backward-sexp
   ">" (sp-restrict-to-object-interactive #'sp-prefix-pair-object 'sp-previous-sexp)
   "<" (sp-restrict-to-object-interactive #'sp-prefix-pair-object 'sp-backward-sexp)
   "(" 'sp-beginning-of-previous-sexp
   ")" 'sp-end-of-previous-sexp
   "j" 'sp-backward-down-sexp
   "k" 'sp-backward-up-sexp)
  ('motion
   'smartparens-mode-map
   "(" 'sp-beginning-of-sexp
   ")" 'sp-end-of-sexp))

(use-package eriks-sp-surround
  :general-config
  ('operator
   'smartparens-mode-map
   "s" 'eriks/sp-surround-operator)
  ('visual
   'smartparens-mode-map
   "s" 'eriks/sp-surround))

(use-package eriks-sp-evil-motions
  :general-config
  ;; NOTE: it doesn't work to bind to 'inner and 'outer with a mode
  ('(operator visual)
   'smartparens-mode-map
   "is" 'eriks/evil-sp-inner-sexp
   "as" 'eriks/evil-sp-a-sexp
   "i%" 'eriks/evil-sp-inner-hybrid-sexp)
  ('motion
   'smartparens-mode-map
   "]%" 'eriks/sp-evil-end-of-hybrid-sexp
   "[%" 'eriks/sp-evil-beg-of-hybrid-sexp
   [remap evil-jump-item] 'eriks/sp-jump-item))

(use-package eriks-sp-post-handlers
  :config
  (eriks/sp-post-handlers 'sh-mode "{" :widen :open)
  (eriks/sp-post-handlers 'sh-mode "[" :widen)
  (eriks/sp-post-handlers 'js-mode '("[" "{") :open)
  (eriks/sp-post-handlers 'rust-mode "{" :open)
  (eriks/sp-post-handlers 'typescript-mode '("[" "{") :open)
  (eriks/sp-post-handlers 'css-mode "{" :open)
  (eriks/sp-post-handlers 'conf-mode '("[" "{") :open)
  (eriks/sp-post-handlers '(c-mode java-mode c++-mode) "{" :open))

(use-package eriks-sp-triple
  :config
  (sp-with-modes '(python-mode)
    (sp-local-pair "'" nil :post-handlers '((:rem sp-python-fix-tripple-quotes)))
    (sp-local-pair "\"" nil :post-handlers '((:rem sp-python-fix-tripple-quotes)))
    (eriks/sp-post-handlers '("'''" "\"\"\"") :open)))
