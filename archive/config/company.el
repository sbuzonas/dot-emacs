(dolist (item '(company company-lua company-php))
  (add-to-list 'slb/packages item))

(defun slb/company-init ()
  (require 'company)
  (global-company-mode))

(if (package-installed-p 'company)
    (slb/company-init)
  (add-hook 'slb/packages-installed 'slb/company-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
