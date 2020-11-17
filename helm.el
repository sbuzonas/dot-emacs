;;; helm.el --- .emacs helm -*- lexical-binding: t  -*-

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

(use-package helm
  :delight
  :init
  (require 'helm-config)
  :config
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-h f" . helm-apropos)
         ("C-x h" . helm-command-prefix)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         :map helm-find-files-map
         ("C-x C-f" . helm-ff-run-toggle-auto-update)
         :map helm-command-map
         ("o" . helm-occur)
         ("g" . helm-do-grep-ag)))

(use-package helm-company
  :after company
  :bind (:map company-mode-map
	 ("C-;" . helm-company)
	 :map company-active-map
	 ("C-;" . helm-company)))

(use-package helm-projectile
  :commands (helm-projectile-on)
  :config
  (helm-projectile-on))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; helm.el ends here
