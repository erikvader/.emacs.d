;; works
(seq-let (a) (list 1)
  (list a))

;; b is nil. I don't know where the doc for this behaviour is
(seq-let (a b) (list 1)
  (list a b))

;; extra is ignored. This macro expands to a pcase-let that uses the seq pattern, and the
;; docs for that says that extra list elements are ok.
(seq-let (a b) (list 1 2 3)
  (list a b))

;; works as expected
(seq-let (a b &rest rest) (list 1 2 3)
  (list a b rest))

;; works as expected
(pcase-let ((`(,a) (list 1)))
  (list a))

;; non-matching pattern is undefined according to the docs, anything can happen
(pcase-let ((`(,a ,b) (list 1)))
  (list a b))

;; non-matching pattern is undefined according to the docs, anything can happen
(pcase-let ((`(,a ,b) (list 1 2 3)))
  (list a b))

;; works
(pcase-let ((`(,a ,b . ,rest) (list 1 2 3)))
  (list a b rest))

;; matches the pattern with b as nil
(pcase (list 1)
  ((seq a b) (list a b))
  (_ t))

;; does not match the pattern
(pcase (list 1)
  (`(,a ,b) (list a b))
  (_ t))

;; does not match the pattern
(pcase (list 1 2 3)
  (`(,a ,b) (list a b))
  (_ t))

;; does match the pattern
(pcase (list 1 2 3)
  (`(,a ,b . ,_) (list a b))
  (_ t))

;; works
(cl-destructuring-bind (a) (list 1)
  (list a))

;; signals error
(cl-destructuring-bind (a b) (list 1)
  (list a b))

;; signals error
(cl-destructuring-bind (a b) (list 1 2 3)
  (list a b))

;; works
(cl-destructuring-bind (a b . c) (list 1 2 3)
  (list a b c))

;; a &rest is equivalent to .
(cl-destructuring-bind (a b &rest c) (list 1 2 3)
  (list a b c))
