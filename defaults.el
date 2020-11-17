;;; defaults.el --- .emacs defaults -*- lexical-binding: t  -*-

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
  (defvar autosave-dir)
  (defvar backup-dir)
  (defvar history-dir)
  (require 'use-package))

;;; Personal info

(setq inhibit-startup-message t)
(setq user-full-name "Steve Buzonas"
      user-mail-address "steve@fancyguy.com"
      organization "FancyGuy Technologies")

;;; Backups

(setq backup-by-copying t) ;; Don't clobber symlinks
(setq backup-directory-alist `(("." . ,backup-dir))) ;; Store backups centrally
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t))) ;; Same for saves
(setq auto-save-list-file-prefix (concat autosave-dir ".saves-"))

;;; Built-in version control

(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq vc-make-backup-files t)

;;; History

(setq history-length t)
(setq history-delete-duplicates t)

(require 'savehist)
(setq savehist-autosave-interval 60)
(setq savehist-file (expand-file-name "minibuffer" history-dir))
(setq savehist-save-minibuffer-history t)

(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)

(savehist-mode t)

(require 'recentf)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 500)
(setq recentf-save-file (expand-file-name "recentf" history-dir))

;;; Appearance

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;;; Editor

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward) ;; provide better naming context

(use-package delight)

(use-package winner
  :defer t)

(use-package undo-tree
  :commands (global-undo-tree-mode)
  :delight
  :config
  (global-undo-tree-mode t))

(require 'linum)
(setq-default linum-format "%4d \u2502")
(set-face-foreground 'linum "#a8a88a")
(set-face-bold 'linum t)

(add-hook 'text-mode-hook #'visual-line-mode)

;; Don't use tabs unless explicitly configured
(setq-default indent-tabs-mode nil)

;; Search & Replace

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Utility functions
(defun slb/disable-window-selection ()
  (interactive)
  (set-window-parameter (first (window-list)) 'no-other-window t))

(setq tramp-default-method "sshx")

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; defaults.el ends here

