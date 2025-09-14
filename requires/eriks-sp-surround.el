(cl-defun eriks/sp-wrap-with (beg end open close target-pos)
  "Like `sp-wrap-with-pair', with the addition of control where the cursor ends up."
  ;;NOTE: based on the lambda from the :wrap argument to `sp-pair'
  (unless (<= beg end)
    (error "Invalid range %s %s" beg end))
  (let* ((org (point))
         (open-len (length open))
         (close-len (length close))
         (inner-beg (+ beg open-len))
         (inner-end (+ open-len end))
         (outer-beg beg)
         (outer-end (+ open-len close-len end))
         (org-after (+ org open-len)))
    (goto-char end)
    (insert close)
    (goto-char beg)
    (insert open)
    (goto-char (or (plist-get (list :after-closing outer-end
                                    :before-closing inner-end
                                    :after-opening inner-beg
                                    :before-opening outer-beg
                                    :save-excursion org-after)
                              target-pos)
                   (error "Invalid TARGET-POS '%s'" target-pos)))
    (sp--indent-region outer-beg outer-end)
    ;; NOTE: indent region doesn't place the point at the correct location if it was at
    ;; back-to-indentation already
    (when (bolp)
      (back-to-indentation))))

(defun eriks/sp-remove-enclosing ()
  "Delete the enclosing parens and return the region they contained.

Returns the region or throws an error."
  (let ((enc (sp-get-enclosing-sexp)))
    (unless enc
      (user-error "Not inside an enclosing paren"))
    (save-excursion
      (sp-get enc
        (goto-char :end)
        (delete-char (- :cl-l))
        (goto-char :beg)
        (delete-char :op-l)
        (list :beg (- :end :op-l :cl-l) :op)))))

;; NOTE: Wrapping for tags has been removed
;; https://github.com/Fuco1/smartparens/commit/f4c0f0da485f9bbbff39e51b4673b035ed1320a6.
;; The code to insert a tag is gone, but deleting, slurping, barfing and navigating by
;; them is still left. There are traces of tag wrapping left in `sp-tags'. It doesn't
;; seem like :when and :unless are supported for tags.
(defun eriks/sp-get-tags-list-wrap ()
  "A variant of `sp--get-pair-list-wrap' that looks at `sp-tags'."
  (->> (alist-get major-mode sp-tags)
       (cl-remove-if-not (lambda (tag) (memq 'wrap (plist-get tag :actions))))
       (mapcar (lambda (tag) (list (plist-get tag :trigger)
                                   ;; NOTE: :transform is only used in sgml tags for a
                                   ;; reason I don't care about, so I disregard it
                                   ;; completely.
                                   (plist-get tag :open)
                                   (plist-get tag :close))))))

(defun eriks/sp-populate-tag (open close)
  "If both OPEN and CLOSE contain exactly one underscore, then prompt the
user for a value to replace it with."
  (let ((so (split-string open "_"))
        (sc (split-string close "_")))
    (if (not (= 2 (length so) (length sc)))
        (cons open close)
      ;; NOTE: I have no clue why, but this `read-from-minibuffer' is not recorded by
      ;; evil unless these extra calls to `evil-repeat-recording-p' are here. Taken from
      ;; https://github.com/emacs-evil/evil-surround/blob/da05c60b0621cf33161bb4335153f75ff5c29d91/evil-surround.el#L123.
      ;; Without this 'post call, all keys are recorded, but in the wrong order for some
      ;; reason.
      (when (evil-repeat-recording-p)
        (evil-repeat-keystrokes 'post))
      (let ((input (read-from-minibuffer (car so))))
        (when (evil-repeat-recording-p)
          (evil-repeat-record input))
        (cons (concat (car so) input (cadr so))
              (concat (car sc) input (cadr sc)))))))

(cl-defun eriks/sp-query-wrappable-pair ()
  "Query the user for a pair to wrap with.

Return the pair to use or throws an error."
  (let ((candidates (append (sp--get-pair-list-wrap)
                            (eriks/sp-get-tags-list-wrap)))
        char
        query
        fullmatches)
    (unless candidates
      (user-error "There are no valid parens to wrap with in this context"))
    (while (setq char (read-char-exclusive))
      (setq query (concat query (list char)))
      (setq candidates (cl-remove-if-not
                        (lambda (pair) (or (string-prefix-p query (car pair))
                                           (and (stringp (cdr pair))
                                                (string-prefix-p query (cdr pair)))))
                        candidates))
      (setq fullmatches (cl-remove-if-not
                         (lambda (pair) (or (equal query (car pair))
                                            (equal query (cdr pair))))
                         candidates))
      (when-let ((pair (cond
                        ((and fullmatches
                              (> (length candidates) 1))
                         (cadddr
                          ;; NOTE: using the long variant breaks evil-repeat, at least
                          ;; with ivy enabled
                          (read-multiple-choice
                           "Ambiguity"
                           (cl-loop for (open . close) in candidates
                                    for c across "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ"
                                    collect (list c
                                                  (format "%s .. %s" open (if (stringp close)
                                                                              close
                                                                            "TAG"))
                                                  "a string that is looking for a purpose"
                                                  (cons open close))))))
                        (fullmatches
                         (car candidates))
                        ((null candidates)
                         (user-error "Could not find pair '%s'" query)))))
        (let* ((open (car pair))
               (close (cdr pair))
               (target (cond
                        ((equal query close) :before-closing)
                        ((not (stringp close)) :after-opening)
                        (t :before-opening))))
          (unless (stringp close)
            (cl-destructuring-bind (o . c)
                (eriks/sp-populate-tag (car close) (cadr close))
              (setq open o
                    close c)))
          (cl-return-from eriks/sp-query-wrappable-pair
            (list open close target)))))))

(evil-add-command-properties 'evil-yank :repeat 'eriks/evil-yank-repeat-handler)
(defun eriks/evil-yank-repeat-handler (flag)
  "Repeat is disabled for `evil-yank', which is a problem for
`eriks/sp-operator' and `eriks/sp-surround' since repeat info is not
recorded at all. This is a special handler for `evil-yank' that disables
recording, like default, unless `eriks/sp-operator' is invoked."
  (evil-repeat-keystrokes flag) ;; NOTE: I think this is what is called on :repeat t
  (when (eq flag 'post)
    (unless (pcase evil-repeat-info
              ;; TODO: put the ?s in a variable or something?
              ((seq (seq _ ?s)) t)
              ((seq `[,_] (seq ?s)) t)
              (_ nil))
      (evil-repeat-abort))))

(evil-define-command eriks/sp-surround-operator ()
  "A dispatcher to call appropriate smartparens functions from, for
example, d in normal state."
  (setq evil-inhibit-operator t)
  (pcase evil-this-operator
    ('evil-delete (call-interactively 'sp-splice-sexp))
    ('evil-change (call-interactively 'eriks/sp-rewrap))
    ('evil-yank (call-interactively 'eriks/sp-surround))
    (_ (user-error "Invalid operator %s" evil-this-operator))))

(evil-define-operator eriks/sp-surround (beg end type)
  "Surround the region or motion with a pair.

Characters are read until a match to an opening or closing delimiter in
`sp-pair-list' with a wrap action is found, then surround with that
pair. If the closing delimiter is pressed, then place the point at it
when inserted, else place it around the opening delimiter.

If a pair with a wrap action in `sp-tags' is found for the current
major-mode and the trigger is entered, then start an insert of a tag
using the minibuffer.

If surrounding lines, then put the delimiters on the line above those
and on the line below."
  (interactive "<R>")
  (cl-destructuring-bind (open close target) (eriks/sp-query-wrappable-pair)
    (when (eq type 'line)
      (goto-char end)
      (save-excursion
        (insert "\n")
        (goto-char beg)
        (insert "\n"))
      (setq end (point)))

    (eriks/sp-wrap-with beg end open close target)
    ;; TODO: do these handlers expect that `sp-last-wrapped-region' is set?
    (sp--run-hook-with-args open :post-handlers 'wrap)))

(evil-define-operator eriks/sp-rewrap ()
  "Rewrap the current delimiters with another pair.

Works like `sp-rewrap-sexp', but uses the power of `eriks/sp-surround'."
  (interactive)
  (pcase-let* ((`(,beg ,end ,parent) (eriks/sp-remove-enclosing))
               (`(,open ,close ,_) (eriks/sp-query-wrappable-pair)))
    (eriks/sp-wrap-with beg end open close :save-excursion)
    ;; TODO: do these handlers expect that `sp-last-wrapped-region' is set?
    (sp--run-hook-with-args open :post-handlers 'rewrap-sexp
                            (list :parent parent))))

(provide 'eriks-sp-surround)
