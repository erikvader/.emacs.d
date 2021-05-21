(defun eriks/describe-keymap (map)
  "Nicely shows all bindings in map"
  (interactive "sMap: ")
  (when map
    (with-output-to-temp-buffer "*describe keymap*"
      (princ (substitute-command-keys (format "\\{%s}" map))))))

(defun eriks/rename-current-file ()
  "Rename the file the current buffer is currently visiting."
  (interactive)
  (let ((current-name (buffer-file-name)))
    (unless current-name
      (user-error "current buffer is not visiting a file"))
    (let ((new-name (read-file-name "rename to " nil nil nil nil nil)))
      ;;TODO: `expand-file-name'??
      (when new-name
        (rename-file current-name new-name nil)
        (set-visited-file-name new-name nil t)))))

(defalias 'rename-current-file-n-buffer #'eriks/rename-current-file)

(defun eriks/diction-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "diction -L en_GB -bs"))
