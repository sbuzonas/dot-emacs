(eval-when-compile
  (require 'projectile))

(dolist (item '(projectile org-projectile))
  (add-to-list 'slb/packages item))

(defun slb/projectile-init ()
  (require 'projectile)
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c C-d") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-switch-project))

(if (package-installed-p 'projectile)
    (slb/projectile-init)
  (add-hook 'slb/packages-installed 'slb/projectile-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
