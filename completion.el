;;; completion.el --- .emacs completion -*- lexical-binding: t  -*-

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
  (defvar history-dir)
  (require 'use-package))

(use-package company
  :commands (global-company-mode)
  :delight
  :init
  (global-company-mode))

(use-package ido
  :commands (ido-mode)
  :config
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-max-prospects 10)
  (setq ido-save-directory-list-file (expand-file-name "ido" history-dir))
  (setq ido-default-file-method 'selected-window)
  (setq ido-auto-merge-work-directories-length -1)
  (ido-mode t))

(use-package ido-completing-read+
  :commands (ido-ubiquitous-mode)
  :config
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :commands (flx-ido-mode)
  :config
  (flx-ido-mode t))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; completion.el ends here
