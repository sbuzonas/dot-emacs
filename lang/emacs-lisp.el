;;; emacs-lisp.el --- configuration for emacs-lisp -*- lexical-binding: t  -*-

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

(setq load-prefer-newer t)

(defvar site-source-dir "/usr/local/src")
(defvar emacs-short-version (format "%d.%d" emacs-major-version emacs-minor-version))
(defalias 'short-version 'emacs-short-version)

(let ((emacs-release (format "emacs-%s" emacs-short-version)))
  (setq source-directory (expand-file-name emacs-release site-source-dir)))

(defun slb/emacs-lisp-mode-hook ()
  (eldoc-mode t)
  (rainbow-mode 1))

(use-package auto-compile
  :commands (auto-compile-on-load-mode auto-compile-on-save-mode)
  :config
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

(use-package elisp-mode
  :ensure nil
  :pin manual
  :delight (emacs-lisp-mode ("Elisp" (lexical-binding ":Lex" ":Dyn")) :major)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook 'slb/emacs-lisp-mode-hook))

(use-package cask-mode)

(use-package eldoc
  :delight)

(use-package flycheck
  :hook (emacs-lisp . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-emacs-lisp-check-declare t))

(use-package flycheck-cask
  :hook (cask-mode . flycheck-mode))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; emacs-lisp.el ends here
