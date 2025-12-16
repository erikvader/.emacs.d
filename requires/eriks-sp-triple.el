;; TODO: There is a bug when doing `sp-down-sexp' into a triple quote, it errors with
;; "sp--find-next-stringlike-delimiter: Invalid search bound (wrong side of point)". Not
;; sure if there are more advices that need to be added somewhere, or if this is an actual
;; bug. This happens even if these advices aren't loaded, so probably the latter.

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
instead of three."
  (when (and (eq action 'skip-closing-pair)
             (eriks/looking-back-double id))
    (self-insert-command 1 (aref id 0))))

(define-advice sp-skip-into-string (:after (&optional back) triple-quote)
  "This advice makes this function go further over triple quotes.

When run over a triple quote this function stops after the first quote,
and this advice essentially checks whether there are two quotes more and
skips those as well."
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
  "This advice extends the bounds on both sides to include triple quotes."
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

(provide 'eriks-sp-triple)
