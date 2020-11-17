(eval-when-compile
  (require 'cc-mode)
  (require 'company))

(dolist (item '(omnisharp))
  (add-to-list 'slb/packages item))

(defun slb/omnisharp-init ()
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp)))

(defun slb/csharp-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (local-set-key (kbd "C-c C-c") 'recompile))
(add-hook 'csharp-mode-hook 'slb/csharp-mode-setup t)

(if (package-installed-p 'omnisharp)
    (slb/omnisharp-init)
  (add-hook 'slb/packages-installed 'slb/omnisharp-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
