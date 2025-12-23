;; TODO: the highlighting of the matching pair when the cursor is on top of a delimiter
;; does'nt work all the time for triple quotes.

(defun eriks/looking-back-double (char)
  "Returns non-nil if looking back at two CHAR, not three."
  (unless (= 1 (length char))
    (error "Char string contains muliple chars %s" char))
  (looking-back
   (rx-to-string '(| (: buffer-start
                        (eval char)
                        (eval char))
                     (: (not (eval char))
                        (eval char)
                        (eval char)))
                 t)))

(defun eriks/looking-at-double (char)
  "Returns non-nil if looking at two CHAR, not three."
  (unless (= 1 (length char))
    (error "Char string contains muliple chars %s" char))
  (looking-at
   (rx-to-string '(| (: (eval char)
                        (eval char)
                        buffer-end)
                     (: (eval char)
                        (eval char)
                        (not (eval char))))
                 t)))

(defun eriks/sp-triple-quote-post-handler (id action _context)
  "This makes it possible to insert a triple quote with only two keypresses
instead of three. Use this as a post-handler for the single quote."
  (when (and (eq action 'skip-closing-pair)
             (eriks/looking-back-double id))
    (self-insert-command 1 (aref id 0))))

(define-advice sp-skip-into-string (:after (&optional back) triple-quote)
  "This advice makes this function go further over triple quotes.

When run over a triple quote this function stops after the first quote,
and this advice essentially checks whether there are two quotes more and
skips those as well.

This should fix `sp-get-string' as it is the only function that uses
this function."
  (when-let* ((bounds (sp-get-quoted-string-bounds))
              (start (car bounds))
              (end (cdr bounds))
              (quot (string (char-after start))))
    (cond ((and back
                (= (point) (- end (length quot)))
                (eriks/looking-back-double quot))
           (backward-char (* 2 (length quot))))
          ((and (not back)
                (= (point) (+ start (length quot)))
                (eriks/looking-at-double quot))
           (forward-char (* 2 (length quot)))))))

(define-advice sp--get-string (:filter-return (plist) triple-quote)
  "This advice extends the bounds on both sides to include triple quotes.

This fixes `sp-get-string' as that is the only function that uses this
function."
  (if-let* (plist
            (op (plist-get plist :op))
            (triple-op (concat op op op))
            (triple-cl (sp-get-pair triple-op :close))
            ((equal triple-op triple-cl))
            (beg (plist-get plist :beg))
            (end (plist-get plist :end))
            ((save-excursion
               (goto-char beg)
               (eriks/looking-back-double op)))
            ((save-excursion
               (goto-char end)
               (eriks/looking-at-double op))))
      (list :beg (- beg (* 2 (length op)))
            :end (+ end (* 2 (length op)))
            :op triple-op
            :cl triple-op
            :prefix ""
            :suffix "")
    plist))

(define-advice sp-get-quoted-string-bounds (:filter-return (begend) triple-quote)
  "This advice extends the bounds on both sides to include triple quotes.

This fixes `sp-get-stringlike-expression' and hopefully doesn't ruin anything else."
  (if-let* (begend
            (beg (car begend))
            (end (cdr begend))
            (op (string (char-after beg)))
            ((save-excursion
               (goto-char beg)
               (eriks/looking-back-double op)))
            ((save-excursion
               (goto-char end)
               (eriks/looking-at-double op))))
      (cons (- beg (* 2 (length op)))
            (+ end (* 2 (length op))))
    begend))

(provide 'eriks-sp-triple)
