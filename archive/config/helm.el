(eval-when-compile
  (require 'company))

(dolist (item '(helm helm-company))
  (add-to-list 'slb/packages item))

(defun slb/helm-init ()
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1)
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-;") 'helm-company)
       (define-key company-active-map (kbd "C-;") 'helm-company))))

(if (package-installed-p 'helm)
    (slb/helm-init)
  (add-hook 'slb/packages-installed 'slb/helm-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
