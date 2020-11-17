;;; terraform.el --- configuration for terraform -*- lexical-binding: t  -*-

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

;; (defun slb/typescript-init ()
;;   (flycheck-mode t)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode t)
;;   (yas-minor-mode)
;;   (slb/use-ts-executables-from-node-modules))

;; (defun slb/tide-init ()
;;   (tide-setup)
;;   (tide-hl-identifier-mode t))

;; (defun slb/tide-web-init ()
;;   (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;     (slb/tide-init)
;;     (yas-minor-mode)
;;     (yas-activate-extra-mode 'typescript-mode)))

(use-package company-terraform)

(use-package terraform-mode
  :delight " TF")

;; (use-package tide
;;   :commands (tide-setup tide-hl-identifier-mode)
;;   :delight " TSS"
;;   :init
;;   (add-hook 'typescript-mode-hook #'slb/tide-init)
;;   (add-hook 'web-mode-hook #'slb/tide-web-init))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; typescript.el ends here
