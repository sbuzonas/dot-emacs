;;; common-lisp.el --- configuration for common-lisp -*- lexical-binding: t  -*-

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

(use-package slime
  :mode ("\\.cl\\'" . lisp-mode)
  :mode ("\\.sbclrc\\'" . lisp-mode)
  :bind (:map slime-mode-map
         ("TAB" . slime-indent-and-complete-symbol)
         ("C-c C-s" . slime-selector))
  :config
  (setq slime-contribs '(slime-fancy))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq slime-fuzzy-completion-in-place t)
  (setq slime-enable-evaluate-in-emacs t)
  (setq slime-autodoc-use-multiline-p t)
  (setq slime-auto-start 'always))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; common-lisp.el ends here
