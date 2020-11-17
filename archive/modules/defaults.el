;; -*- lexical-binding: t  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(setq user-full-name "Steve Buzonas"
      user-mail-address "steve@fancyguy.com")

;; Don't use tabs unless explicitly configured
(setq-default indent-tabs-mode nil)

;; (setq apropos-do-all t ;; get more extensive results from apropos
(setq mouse-yank-at-point t ;; insert at the point rather than the click
;;      ediff-window-setup-function 'ediff-setup-windows-plain ;; use existing frame
      visible-bell t
      require-final-newline t)

;; Prefer regexp variants for search and replace
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; provide better context to buffers of the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; OSX Key modifications
(setq ns-alternate-modifier 'alt)
(setq ns-command-modifier nil)
(setq ns-control-modifier 'control)
(setq ns-function-modifier 'hyper)

(setq ns-right-alternate-modifier 'meta)
(setq ns-right-command-modifier nil)
(setq ns-right-control-modifier nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:
