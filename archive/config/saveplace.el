(add-to-list 'slb/packages
             'saveplace)

(defun slb/saveplace-init ()
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (concat data-home "/saveplace")))

(if (package-installed-p 'saveplace)
    (slb/saveplace-init)
  (add-hook 'slb/packages-installed 'slb/saveplace-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
