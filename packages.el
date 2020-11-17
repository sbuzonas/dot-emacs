;;; packages.el --- configuration for package.el -*- no-byte-compile: t  -*-

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

(require 'package)
(defalias 'package--ensure-init-file #'(lambda()))
(setq package--init-file-ensured t)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa"         . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("org"          . 10)
        ("elpa"         . 5)
        ("melpa"        . 2))
      package-pinned-packages
      '((use-package . "melpa")))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; packages.el ends here
