(defvar eriks/counsel-grep-function #'counsel-rg
  "The search function to use, needs to have the same signature
as `counsel-rg' and `counsel-ag'.")
(put 'eriks/counsel-grep-function 'safe-local-variable #'functionp)

(defvar eriks/counsel-grep-extra-args nil
  "Extra arguments to use with `counsel-rg-base-command',
depending on `eriks/counsel-grep-function'. This is intended to
be used as a file local variable.")
(put 'eriks/counsel-grep-extra-args 'safe-local-variable #'listp)

(defun eriks/counsel-grep (arg)
  "A version of `counsel-ag' that uses `projectile-acquire-root'
to find a suitable starting point instead of only considering git
repos."
  (interactive "p")
  (let (init-dir
        (prompt (concat (case eriks/counsel-grep-function
                          (#'counsel-rg "rg")
                          (#'counsel-ag "ag")
                          (t "??"))
                        ": "))
        (extra-args (when (and (< arg 16)
                               eriks/counsel-grep-extra-args)
                      (mapconcat #'shell-quote-argument
                                 eriks/counsel-grep-extra-args
                                 " "))))
    (when (< arg 4)
      (let ((proj (projectile-acquire-root)))
        (setq init-dir proj
              prompt (format "[%s] %s" (projectile-project-name proj) prompt))))
    (funcall eriks/counsel-grep-function nil init-dir extra-args prompt)))

(provide 'eriks-counsel-grep)
