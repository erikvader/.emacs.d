(defvar eriks/counsel-grep-initial-directory 'projectile
  "The initial directory to use with `eriks/counsel-grep'. A
value of nil uses the default of `counsel-rg' (first git repo), a
string is a path to a directory to use, and the symbol
'projectile will use `projectile-project-root' to find the
initial directory.")
(defun eriks/grep-init-dir-p (x)
  "Determines whether a X is valid for `eriks/counsel-grep-initial-directory'."
  (or (null x) (eq x 'projectile) (stringp x)))
(put 'eriks/counsel-grep-initial-directory 'safe-local-variable #'eriks/grep-init-dir-p)

(defvar eriks/counsel-grep-function #'counsel-rg
  "The search function to use, needs to have the same signature
as `counsel-rg' or `counsel-ag'.")
(put 'eriks/counsel-grep-function 'safe-local-variable #'functionp)

(defvar eriks/counsel-grep-extra-args nil
  "Extra arguments to use with `counsel-rg-base-command'.")
(put 'eriks/counsel-grep-extra-args 'safe-local-variable #'listp) ;;TODO: list of strings

(defun eriks/counsel-grep (arg)
  (interactive "p")
  ;;TODO: triple C-u: ask what projectile project to search in
  (let* ((init-dir (cond ((>= arg 4)
                          nil)
                         ((eq eriks/counsel-grep-initial-directory 'projectile)
                          (or
                           (projectile-project-root)
                           default-directory))
                         (t
                          eriks/counsel-grep-initial-directory)))
         (prompt (when (and (eq eriks/counsel-grep-initial-directory 'projectile)
                            (projectile-project-p init-dir))
                   (projectile-prepend-project-name "grep: ")))
         (extra-args (when (< arg 16)
                       (mapconcat #'shell-quote-argument
                                  eriks/counsel-grep-extra-args
                                  " "))))
    (funcall eriks/counsel-grep-function nil init-dir extra-args prompt)))

(provide 'eriks-counsel-grep)
