;; repeat-mode is not supported in general, ever: https://github.com/noctuid/general.el/issues/552
;; TODO: repeat-mode doesn't seem to work with evil motions
;; TODO: beware of `repeat-check-key'. Set the property for all commands mention here?
(cl-defmacro eriks/defkey-repeat (map-name-or-args &rest general-keydefs)
  "Bind keys that can be repeated, powered by `repeat-mode'. Bind all keys
as per `general-define-key', but also create a keymap named MAP-NAME
that consists of the same keys, so they can be repeated.

The map is created with `defvar-keymap' and all KEYMAP-KWARGS are
keyword arguments forwarded to it. The list ENTER-LIST is list of
symbols to add the repeat property to, i.e., those functions will enable
the repeat map when called, but without adding or changing their current
binding. If only MAP-NAME is given, then the parenthesis are optional.

GENERAL-KWARGS are keyword arguments as given to `general-define-key',
the important ones are :states, :keymaps and :prefix.

The rest of the arguments are pairs of key binding definitions, KEY is a
string and DEFINITION is a symbol to a function. A pair can be replaced
with a list where the first two elements are KEY and DEFINITION,
followed by keyword arguments. The keywords :hint and :exit have the
same meaning as in `defvar-keymap' under :repeat, but :short is new and
is an alternate binding for DEFINITION to use instead of KEY in the
repeat map. In the list variant is KEY allowed to be nil if SHORTKEY is
specified, this will have bind DEFINITION to the repeat map only.

Simple example:
(eriks/defkey-repeat moved-transpose
  :prefix \"g\"
  \"t\" 'transpose-chars)

This will bind `transpose-chars' to the sequence \"g t\", and this
command can be repeated by pressing \"t\" over and over, like \"g t t
t\".

\(fn (MAP-NAME [:enter ENTER-LIST] [KEYMAP-KWARGS]...) [GENERAL-KWARGS]... [KEY DEFINITION]... \
[(KEY DEFINITION [:hint HINT] [:exit t] [:short SHORTKEY])]...)"
  (declare (indent defun))
  (let (map-name map-args general-args general-keys map-keys keys hints exit enter cur)
    (cond ((symbolp map-name-or-args)
           (setq map-name map-name-or-args))
          ((listp map-name-or-args)
           (setq map-name (car map-name-or-args)
                 map-args (cdr map-name-or-args)))
          (t (error "Invalid map name")))
    (setq map-name (intern (concat (symbol-name map-name) "-repeat-map")))

    (when-let ((ent (cl-getf map-args :enter)))
      (setq enter (eval ent))
      (unless (listp enter)
        (error "The key :enter must be a list"))
      (cl-remf map-args :enter))

    (while general-keydefs
      (setq cur (pop general-keydefs))

      (cond ((listp cur)
             (let* ((k (pop cur))
                    (cmd (pop cur))
                    (cmd-unquote (cadr cmd))
                    (h (cl-getf cur :hint))
                    (e (cl-getf cur :exit))
                    (short (cl-getf cur :short)))
               (unless (eq 'quote (car-safe cmd))
                 (error "Must be a function"))
               (unless (or (null k) (stringp k))
                 (error "Must be a string"))
               (if short
                   (progn
                     (push cmd map-keys)
                     (push short map-keys)
                     (when k
                       (push cmd general-keys)
                       (push k general-keys)))
                 (push cmd keys)
                 (push k keys))
               (when h
                 (push (cons cmd-unquote h) hints))
               (when e
                 (push cmd-unquote exit))))
            ((keywordp cur)
             (push (pop general-keydefs) general-args)
             (push cur general-args))
            ((stringp cur)
             ;;TODO: make sure that cur is only one key long
             (let ((cmd (pop general-keydefs)))
               (unless (eq 'quote (car-safe cmd))
                 (error "Must be a function %s" cmd))
               (push cmd keys)
               (push cur keys)))
            (t (error "Not a valid keydef"))))

    `(progn
       (makunbound ',map-name)
       (general-define-key ,@general-args ,@general-keys ,@keys)
       (defvar-keymap ,map-name :repeat (:enter ,enter :exit ,exit :hints ,hints)
                      ,@map-args ,@map-keys ,@keys))))

(cl-defmacro eriks/defkey-repeat-1 (&rest general-keydefs)
  "Define several independent repeating keybinds.

See also `eriks/defkey-repeat' for a variant that creates multiple bindings in one map.

\(fn [GENERAL-KWARGS]... [KEY DEFINITION]...)"
  (declare (indent defun))
  (cl-loop for (k v) on general-keydefs by #'cddr
           when (null v) do (error "Uneven amount of arguments")
           if (keywordp k)
           append (list k v) into kwargs
           else
           collect `(eriks/defkey-repeat ,(gensym "eriks/repeat-1-") ,@kwargs ,k ,v) into forms
           finally return `(progn ,@forms)))

(provide 'eriks-defkey-repeat)
