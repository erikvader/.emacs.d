(use-package magit
  :ensure t
  :after evil
  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "s" 'magit-status
   "b" 'magit-blame)
  :config
  (add-to-list 'evil-buffer-regexps
               '("^COMMIT_EDITMSG$" . insert))
  :gfhook ('magit-blame-mode-hook #'evil-emacs-state))

(use-package git-timemachine
  :ensure t
  :after evil
  :config
  (add-to-list 'evil-buffer-regexps
               '("^timemachine:" . emacs))
  (eriks/hotfix
   "git-timemachine"
   '(20190317 1547)
   ;; https://gitlab.com/pidu/git-timemachine/issues/77
   (defun fixed-git-timemachine-show-commit ()
     (interactive)
     (let ((rev (car git-timemachine-revision)))
       (if (fboundp 'magit-revision-mode)
           (with-temp-buffer
             (save-excursion
               (magit-setup-buffer #'magit-revision-mode nil
                 (magit-buffer-revision rev)
                 (magit-buffer-range (format "%s^..%s" rev rev))
                 (magit-buffer-diff-args nil)
                 (magit-buffer-diff-files nil))))
         (message "You need to install magit to show commit"))))
   (advice-add #'git-timemachine-show-commit :override #'fixed-git-timemachine-show-commit))
  :general
  ('normal
   :prefix eriks/leader
   "gt" 'git-timemachine))

(use-package what-the-commit
  :ensure t
  :after evil
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "w" 'what-the-commit-insert))

(use-package git-gutter-fringe+
  :ensure t
  :diminish git-gutter+-mode
  :custom
  (git-gutter-fr+-side 'right-fringe)
  :config
  (global-git-gutter+-mode 1)
  :general
  ('normal
   :prefix eriks/leader
   :infix "g"
   "g" 'git-gutter+-mode
   "j" 'git-gutter+-next-hunk
   "k" 'git-gutter+-previous-hunk
   "r" 'git-gutter+-revert-hunk))
