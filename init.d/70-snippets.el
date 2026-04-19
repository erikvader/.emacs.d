(use-package abbrev
  :custom
  (save-abbrevs nil)
  :config
  (defalias 'eriks/abbrev-before-point-p 'abbrev--before-point)
  (defun eriks/abbrev-syntax-hack-advice (fun &rest args)
    "Make more characters available for use in an abbrev.

Only characters that are of class word constituent can be used as an
abbrev, so this advice temporarily makes all characters in all abbrevs
in the current mode into word characters during expansion."
    ;; TODO: cache this copied syntax table? How to know if it has been invalidated?
    ;; TODO: There's also a :regex that can be set on the abbrev table, might be of interest?
    (with-syntax-table (copy-syntax-table (syntax-table))
      (dolist (tbl (abbrev--active-tables))
        (mapatoms (lambda (a)
                    (seq-doseq (char (symbol-name a))
                      (modify-syntax-entry char "w")))
                  tbl))
      (apply fun args)))
  ;; NOTE: There are more functions in the abbrev file that depend on the syntax table,
  ;; but it seems like only these two are needed for my purposes, which is to manually
  ;; invoke `expand-abbrev', the others take care of suggestions and dynamically adding
  ;; new abbrevs, mostly.
  (advice-add 'eriks/abbrev-before-point-p :around 'eriks/abbrev-syntax-hack-advice)
  (advice-add 'expand-abbrev :around 'eriks/abbrev-syntax-hack-advice)

  ;; NOTE: An abbrev doesn't care about case, and it doesn't seem like there is a config
  ;; variable to make it case sensitive, so having anything other than lower case letters
  ;; in the abbrev string is pointless.
  (defmacro eriks/define-abbrev-skeleton (tables abbrev doc &rest skel-body)
    "Define a skeleton and associate it with an abbrev.

The return value is the symbol of the created skeleton."
    (declare (indent defun)
             (doc-string 3))
    (let* ((tables (ensure-list tables))
           (names (mapcar (lambda (table)
                            (intern (concat "eriks/skeleton:" (symbol-name table) ":" abbrev)))
                          tables))
           (extra (seq-mapn (lambda (name table)
                              `(progn (defalias ',name ',(car names))
                                      (define-abbrev ,table ,abbrev "" ',name)))
                            (cdr names)
                            (cdr tables))))
      `(prog1 (define-skeleton ,(car names) ,doc ,@skel-body)
         (define-abbrev ,(car tables) ,abbrev "" ',(car names))
         ,@extra)))

  ;; NOTE: These should probably belong to `eriks/editable-file-hook' somehow, because not
  ;; every mode that has comments belong to `prog-mode-abbrev-table' unfortunately. But
  ;; not everything in my hook is guaranteed to have comments either... So it's easier to
  ;; just add these globally and call it a day. The majority of files I edit do have
  ;; comments, and these won't break anything in files that don't.
  (eriks/define-abbrev-skeleton global-abbrev-table "no"
    "Insert NOTE"
    nil comment-start "NOTE: ")
  (eriks/define-abbrev-skeleton global-abbrev-table "to"
    "Insert TODO"
    nil comment-start "TODO: ")

  :general-config
  ('insert
   :predicate '(eriks/abbrev-before-point-p)
   "TAB" 'expand-abbrev))

(use-package autoinsert
  :custom
  (auto-insert-alist nil)
  (auto-insert-query nil)
  (auto-insert t)
  :config
  ;; C++
  (define-auto-insert '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
    '((replace-regexp-in-string "[^A-Z0-9]" "_"
                                (string-replace "+" "P"
                                                (upcase (file-name-nondirectory buffer-file-name))))
      "#ifndef " str "\n"
      "#define " str "\n\n"
      _
      "\n\n#endif"))

  ;; emacs lisp
  (define-skeleton eriks/elisp-lexical-binding-skeleton
    "Insert lexical binding header" nil
    ";; -*- lexical-binding: t; -*-\n\n"
    '(setq-local lexical-binding t))

  (define-auto-insert 'emacs-lisp-mode 'eriks/elisp-lexical-binding-skeleton)

  (define-auto-insert `(,(concat "\\`"
                                 (regexp-quote (expand-file-name user-emacs-directory))
                                 "requires/.+\\.el\\'")
                        . "Elisp file in requires/")
    [eriks/elisp-lexical-binding-skeleton
     ((file-name-nondirectory (file-name-sans-extension buffer-file-name))
      _ "\n\n"
      "(provide '" str ")")])

  (auto-insert-mode 1))
