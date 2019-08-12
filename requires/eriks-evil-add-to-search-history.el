(evil-define-operator eriks/evil-add-to-search-history (beg end)
  "Start a search for selected text. If this text is at word
boundaries, then surround the searched text with \"\\ <\" (no space)
and \"\\>\". This operator only works if `evil-search-module' is set
to evil-search."
  (when (eq evil-search-module 'evil-search)
    (let ((text (regexp-quote (buffer-substring beg end)))
          (is-word (and
                    (progn
                      (goto-char beg)
                      (looking-at-p "\\<"))
                    (progn
                      (goto-char end)
                      (looking-at-p "\\>")))))
      (when is-word
        (setq text (concat "\\<" text "\\>")))
      (goto-char beg)
      (add-to-history 'evil-ex-search-history text)
      (when (boundp 'swiper-history)
        (cl-pushnew text swiper-history))
      (setq evil-ex-search-pattern (list text t t))
      (setq evil-ex-search-direction 'forward)
      (when evil-ex-search-persistent-highlight
        (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

(provide 'eriks-evil-add-to-search-history)
