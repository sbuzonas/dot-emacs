(setq-default linum-format "%4d\u2502")

(global-linum-mode t)
(global-hl-line-mode t)

;; (set-face-foreground 'linum "#4d4d4d")
;; (set-face-background 'linum "#a8a8a8")
(set-face-foreground 'linum "#a8a8a8")

(defun slb/linum--line-at-click ()
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
          (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (forward-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      (1+ (line-number-at-pos)))))

(defun slb/linum-mouse-down ()
  (interactive)
  (message "Mouse down '%s'" (slb/linum--line-at-click)))

(defun slb/linum-mouse-up ()
  (interactive)
  (message "Mouse up '%s'" (slb/linum--line-at-click)))

;;(global-set-key (kbd "<left-margin> <down-mouse-1>") 'slb/linum-mouse-down)
;;(global-set-key (kbd "<left-margin> <mouse-1>") 'slb/linum-mouse-up)


;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
