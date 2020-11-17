;;; snippets.el --- .emacs snippets -*- lexical-binding: t -*-

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
  (require 'yasnippet))

(defvar slb/yas-snippets-dir
  "Directory where snippets are stored.")

(defvar slb/yas-minor-modes '()
  "List of hooks of major modes in which `yas-minor-mode' should be enabled.")

(defun inflect--mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun inflect--separate-words (s)
  "Split into a list of words."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun inflect-camel-case (s)
  (mapconcat 'identity (inflect--mapcar-head
                        #'(lambda (word) (downcase word))
                        #'(lambda (word) (capitalize (downcase word)))
                        (inflect--separate-words s)) ""))

(defun inflect-capitalized-words (s)
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (inflect--separate-words s)) " "))

(defun inflect-slug-case (s)
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (downcase word))
                        (inflect--separate-words s)) "-"))

(defun inflect-snake-case (s)
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (downcase word))
                        (inflect--separate-words s)) "_"))

(defun inflect-title-case (s)
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (inflect--separate-words s)) ""))

(use-package yasnippet
  :commands (yas-activate-extra-mode)
  :defer 20
  :bind (:map slb-mode-map
              ("s-y" . hydra-yas/body)
              ("C-c y" . hydra-yas/body))
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt
                                 yas-completing-prompt))
    (setq slb/yas-snippets-dir (let ((dir (concat config-home
                                                  "/snippets/")))
                                 (make-directory dir t)
                                 dir))
    (setq yas-snippet-dirs (list slb/yas-snippets-dir))

    (setq yas-new-snippet-default "# -*- mode: snippet -*-
# contributor: `user-full-name`
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((yas-indent-line 'auto) (yas-also-auto-indent-first-line t) (yas-wrap-around-region t))}
# --
$0")

    (defun slb/turn-on-yas-minor-mode ()
      "Turn on yas-minor-mode only for specific modes."
      (interactive)
      (yas-reload-all)
      (dolist (hook slb/yas-minor-modes)
        (add-hook hook #'yas-minor-mode)))

    (defun slb/turn-off-yas-minor-mode ()
      "Turn off yas-minor-mode only for specific modes."
      (interactive)
      (yas-reload-all)
      (dolist (hook slb/yas-minor-modes)
        (remove-hook hook #'yas-minor-mode)))

    (slb/turn-on-yas-minor-mode)

    (defhydra hydra-yas (:color blue
                         :hint nil)
      "
[yasnippet]        _i_nsert        _n_ew        _v_isit snippet file        _r_eload all        e_x_pand        _?_ list snippets        "
      ("i" yas-insert-snippet)
      ("n" yas-new-snippet)
      ("v" yas-visit-snippet-file)
      ("r" yas-reload-all)
      ("x" yas-expand)
      ("?" yas-describe-tables)
      ("q" nil "cancel" :color blue))))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; snippets.el ends here
