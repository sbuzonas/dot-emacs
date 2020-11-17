;;; programming.el --- Provide configuration for programming modes -*- lexical-binding: t  -*-

;; Copyright (C) 2015-2018 Steve Buzonas

;; Author: Steve Buzonas <steve@fancyguy.com>
;; Keywords: dotemacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar programming-highlight-task-words
  '("BUG" "FIXME" "HACK" "TODO"))

(defvar programming-highlight-task-face 'font-lock-warning-face)

(defun slb/local-auto-fill-comments ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun slb/font-lock-task-words ()
  (let* ((kw-regex (regexp-opt programming-highlight-task-words))
         (pattern (concat "\\<%s:" kw-regex)))
    (font-lock-add-keywords nil `((,pattern 1 ,programming-highlight-task-face t)))))

(defun slb/prog-mode-hook ()
  (setq truncate-lines t)
  (linum-mode t)
  (column-number-mode t)
  (show-paren-mode t))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :delight
  :init
  (require 'ispell)
  :config
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode)))

(add-hook 'prog-mode-hook #'slb/prog-mode-hook)

(use-package flycheck
  :commands (flycheck-mode)
  :config
  (if (fboundp 'global-flycheck-mode)
      (global-flycheck-mode t)
    (add-hook 'prog-mode-hook #'flycheck-mode)))

(use-package rainbow-mode
  :commands (rainbow-mode)
  :delight
  :config
  (add-hook 'emacs-lisp-mode #'rainbow-mode))

(use-package fill-column-indicator
  :commands (fci-mode)
  :config
  (add-hook 'programming-mode #'fci-mode))

(use-package magit)

(use-package editorconfig
  :delight " EC")

(use-package web-mode)

(require 'lorem-ipsum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; programming.el ends here
