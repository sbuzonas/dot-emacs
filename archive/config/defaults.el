;; Turn off pointless display features
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Highlight matching parens
(show-paren-mode 1)

;; Don't use tabs unless explicitly configured
(setq-default indent-tabs-mode nil)

(setq apropos-do-all t ;; get more extensive results from apropos
      mouse-yank-at-point t ;; insert at the point rather than the click
      load-prefer-newer t ;; load el over elc when el is newer
      ediff-window-setup-function 'ediff-setup-windows-plain ;; use existing frame
      inhibit-startup-screen t ;; don't show the welcom screen
      visible-bell t
      require-final-newline t)

;; Zap-To-Char Enhancement
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-S-z") 'zap-to-char)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

;; Default search keybindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; IDO
(ido-mode t)
(setq ido-save-directory-list-file (concat cache-home "/ido-last")
      ido-enable-flex-matching t)

;; provide better context to buffers of the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; OSX Key modifications
(setq ns-alternate-modifier 'alt)
(setq ns-command-modifier nil)
(setq ns-control-modifier 'control)
(setq ns-function-modifier 'hyper)

(setq ns-right-alternate-modifier 'meta)
(setq ns-right-command-modifier nil)
(setq ns-right-control-modifier nil)

(global-set-key (kbd "M-/") 'hippie-expand)

(column-number-mode 1)

(add-hook 'text-mode-hook
          '(lambda ()
             (visual-line-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
