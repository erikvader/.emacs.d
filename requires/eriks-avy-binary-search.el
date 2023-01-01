(defun eriks/avy-binary-search ()
    "Do a binary search to find a character on the current line"
    (interactive)
    (cl-labels ((b-search (min max)
                          (when (< min max)
                            (let* ((avy-keys '(?j ?k ?l))
                                   points
                                   (middle (floor (+ min max) 2)))
                              (goto-char middle)
                              ;; special case when only two candidates
                              (if (not (= middle min))
                                  (setq points (list min middle max))
                                (setq points (list middle max))
                                (setq avy-keys '(?j ?l)))
                              (avy-process points (avy--style-fn avy-style))
                              (cond
                               ((= (point) middle))
                               ((< (point) middle)
                                (b-search min (1- middle)))
                               ((> (point) middle)
                                (b-search (1+ middle) max)))))))
      (let* ((beg (save-excursion
                    (back-to-indentation)
                    (point)))
             (end (save-excursion
                    (move-end-of-line nil)
                    (point))))
        (b-search beg end))))

(provide 'eriks-avy-binary-search)
