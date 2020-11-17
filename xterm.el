;;; xterm.el --- .emacs xterm -*- lexical-binding: t -*-

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

(defun slb/xterm-title-update ()
  (unless (active-minibuffer-window)
    (send-string-to-terminal (concat "\033]1;" (buffer-name) "\007"))
    (if buffer-file-name
        (send-string-to-terminal (concat "\033]2;" (abbreviate-file-name (buffer-file-name)) "\007"))
      (send-string-to-terminal (concat "\033]2;" (buffer-name) "\007")))))

(defun slb/xterm-title-update--register ()
  (add-hook 'after-init-hook 'slb/xterm-title-update)
  (add-hook 'post-command-hook 'slb/xterm-title-update))

(add-hook 'terminal-init-xterm-hook 'slb/xterm-title-update--register)

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; xterm.el ends here
