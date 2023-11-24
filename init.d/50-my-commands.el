(defun eriks/describe-keymap (map)
  "Nicely shows all bindings in map"
  (interactive "sMap: ")
  (when map
    (with-output-to-temp-buffer "*describe keymap*"
      (princ (substitute-command-keys (format "\\{%s}" map))))))

(defun eriks/diction-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "diction -L en_GB -bs"))

(defun eriks/recompile-elpa ()
  "Compile every .el-file in 'elpa'."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0 t))
