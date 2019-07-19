(defun eriks/avy--regex-candidates-exclusive (regex &optional beg end)
  "same as `avy--regex-candidates' except that all candidates are
  before the actual candidate. Only makes sense/works on the current
  window where the point is."
  (let ((p (point))
        (cur-window (selected-window)))
    (mapcar
     (lambda (a)
       (pcase a
         ((and
           `((,b . ,e) . ,w)
           (guard (equal w cur-window)))
          (cond ((> b p)
                 `((,(1- b) . ,(1- e)) . ,w))
                ((< e p)
                 `((,(1+ b) . ,(1+ e)) . ,w))
                (t
                 `((,b . ,e) . ,w))))
         (_ a)))
     (avy--regex-candidates regex beg end))))

(defun eriks/avy-order-closest-single (x)
  "`avy-order-closest' doesn't like x on the form (p1 . w), but likes ((p1 . p2) . w)"
  (avy-order-closest `((,(car x) . ,(car x)) . ,(cdr x))))

(defun eriks/avy-goto-char-in-line-exclusive (char)
  "Same as `avy-goto-char-in-line' except that it doesn't include the
target character"
  (interactive "cchar:")
  (avy-with avy-goto-char
    (avy--process
     (eriks/avy--regex-candidates-exclusive
      (regexp-quote (string char))
      (line-beginning-position)
      (line-end-position))
     (avy--style-fn avy-style))))

(evil-define-avy-motion eriks/avy-goto-char-in-line-exclusive inclusive)

(defun eriks/avy-goto-line-first-non-blank ()
  "Avy motion to go to the first non-blank character on a chosen line."
  (interactive)
  (avy-with eriks/avy-goto-line-first-non-blank
    (let ((points (nconc (evilem--collect #'evil-next-line-first-non-blank)
                         (evilem--collect #'evil-previous-line-first-non-blank))))
      (avy--process points (avy--style-fn avy-style)))))

(add-to-list 'avy-orders-alist '(eriks/avy-goto-line-first-non-blank . eriks/avy-order-closest-single))
(evil-define-avy-motion eriks/avy-goto-line-first-non-blank line)

(provide 'eriks-evil-avy-motions)
