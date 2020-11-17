(dolist (item '(fill-column-indicator))
  (add-to-list 'slb/packages item))

(defun slb/programming-init ()
  (add-hook 'prog-mode-hook 'turn-on-fci-mode))

(if (package-installed-p 'fill-column-indicator)
    (slb/programming-init)
  (add-hook 'slb/packages-installed 'slb/programming-init))

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; fill-column: 75
;; comment-column: 64
;; End:
