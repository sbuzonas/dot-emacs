;;; pandora.el --- .emacs libraries -*- lexical-binding: t -*-

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
  (require 'use-package)
  (require 'pianobar))

(defvar pianobar-command "/usr/local/bin/pianobar")
(defvar pianobar-auto-hide t)

(defun pianobar-started-p ()
  (when (boundp 'pianobar-buffer)
    (comint-check-proc pianobar-buffer)))

(use-package pianobar
  :ensure nil
  :demand t
  :pin manual
  :commands (pianobar pianobar-window-hide)
  :bind (("<f8>" . pianobar-play-or-pause)
         ("<f9>" . pianobar-next-song)
         ("<f11>" . pianobar-decrease-volume)
         ("<f12>" . pianobar-increase-volume))
  :config

  ;; Advising functions

  (defun slb/pianobar--advise-auto-hide (&rest ignored)
    (when pianobar-auto-hide
      (pianobar-window-hide)))
  (advice-add 'pianobar :after #'slb/pianobar--advise-auto-hide)
  (advice-add 'pianobar-start-process :after #'slb/pianobar--advise-auto-hide)

  (defun slb/pianobar-setup-credentials (&rest ignored)
    (interactive)
    (let ((creds (first (auth-source-search :max 1
                                            :host "pandora.com"
                                            :require '(:user :secret)))))
      (when creds
        (let ((secret (plist-get creds :secret)))
          (when (functionp secret)
            (setq secret (funcall secret)))
          (setq pianobar-username (plist-get creds :user))
          (setq pianobar-password secret)
          (when (plist-member creds :station)
            (setq pianobar-station (plist-get creds :station))
            (when (plist-member creds :volume)
              (setq pianobar-volume (plist-get creds :volume))))))))
  (advice-add 'pianobar-start-process :before #'slb/pianobar-setup-credentials)

  ;; Pianobar utilities

  (defun pianobar-play-or-pause ()
    (interactive)
    (if (pianobar-started-p)
        (pianobar-pause-song)
      (pianobar)))

  ;; Pianobar comint extra

  (defun pianobar-decrease-volume ()
    (interactive)
    (pianobar-send-string "("))

  (defun pianobar-increase-volume ()
    (interactive)
    (pianobar-send-string ")"))

  (defun pianobar-reset-volume ()
    (interactive)
    (pianobar-send-string "^")))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; pandora.el ends here
