(defun eriks/evil-paste--translate-register (register)
  (cond ((eql register ?\") nil)
        ((null register) ?0)
        (t register)))

(evil-define-command eriks/evil-paste-after (count &optional register yank-handler)
  "The same as `evil-paste-after', but uses 0 as the default register instead of \""
  :supress-operator t
  (interactive "*P<x>")
  (setq this-command 'evil-paste-after) ;NOTE: to make `evil-paste-pop' work
  (evil-paste-after count
                    (eriks/evil-paste--translate-register register)
                    yank-handler))

(evil-define-command eriks/evil-paste-before (count &optional register yank-handler)
  "The same as `evil-paste-before', but uses 0 as the default register instead of \""
  :supress-operator t
  (interactive "*P<x>")
  (setq this-command 'evil-paste-before) ;NOTE: to make `evil-paste-pop' work
  (evil-paste-before count
                    (eriks/evil-paste--translate-register register)
                    yank-handler))

(evil-define-operator eriks/evil-yank-delete (beg end type register yank-handler)
  "The same as \"0d"
  (interactive "<R><x><y>")
  ;;TODO: evil-delete does some comparison against `evil-this-operator' for some weird
  ;;vi-behaviour, should this wrapper override that variable to preserve that behaviour?
  (evil-delete beg
               end
               type
               (eriks/evil-paste--translate-register register)
               yank-handler))

(provide 'eriks-evil-default-register)
