(eval-when-compile
  (require 'flycheck))

(dolist (item '(typescript-mode tide tss typescript-mode web-mode))
  (add-to-list 'slb/packages item))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (editorconfig-mode 1)
  (tide-hl-identifier-mode 1))
;;(add-hook 'before-save-hook 'tide-format-before-save)
;;(add-hook 'before-save-hook 'indent-buffer)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun slb/typescript-init ()
  (require 'web-mode)
  (require 'flycheck)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

(if (package-installed-p 'web-mode)
    (slb/typescript-init)
  (add-hook 'slb/packages-installed 'slb/typescript-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
