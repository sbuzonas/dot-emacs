(eval-when-compile
  (require 'folding))

(dolist (item '(folding))
  (add-to-list 'slb/packages item))

(defun slb/folding-setup ()
  (slb/folding-define-keys))

(defun slb/folding-define-keys ()
  (define-key folding-mode-map (kbd "C-c f c") 'folding-hide-current-entry)
  (define-key folding-mode-map (kbd "C-c f C-c") 'folding-hide-current-subtree)
  
  (define-key folding-mode-map (kbd "C-c f u") 'folding-show-current-entry)
  (define-key folding-mode-map (kbd "C-c f C-u") 'folding-show-current-subtree)
  (define-key folding-mode-map (kbd "C-c f M-u") 'folding-show-all)

  (define-key folding-mode-map (kbd "C-c C-f") 'folding-toggle-show-hide)

  (define-key folding-mode-map (kbd "C-c f r") 'folding-fold-region))

(defun slb/folding-init ()
  (require 'folding)
  (folding-mode-add-find-file-hook)
  
  (setq folding-narrow-by-default nil)
  
  (eval-after-load 'folding #'slb/folding-setup))

(if (package-installed-p 'folding)
    (slb/folding-init)
  (add-hook 'slb/packages-installed 'slb/folding-init))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
