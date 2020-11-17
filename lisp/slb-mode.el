;;; slb-mode.el --- slb-mode -*- lexical-binding: t  -*-

;; Copyright (C) 2015-2018 Steve Buzonas

;; Author: Steve Buzonas <steve@fancyguy.com>
;; Keywords: keybindings

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

;; Main use is to have my key bindings have the highest priority

;;; Code:

(defvar slb-special-keymap-prefix (kbd "C-x m")
  "`slb-mode' keymap prefix.
Overrides the default binding for `compose-mail'.")

(defvar slb-mode-special-map (make-sparse-keymap)
  "Special keymap for `slb-mode' whose bindings begin with
`slb-special-keymap-prefix'.")
(fset 'slb-mode-special-map slb-mode-special-map)

(defvar slb-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map slb-special-keymap-prefix 'slb-mode-special-map)
                        map)
  "Keymap for `slb-mode'.")

;;;###autoload
(define-minor-mode slb-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-slb-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter    " μ"
  :keymap     slb-mode-map)

;;;###autoload
(define-globalized-minor-mode global-slb-mode slb-mode slb-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((slb-mode . ,slb-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-slb-mode ()
  "Turn off slb-mode."
  (slb-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-slb-mode)

(defmacro bind-to-slb-map (key fn)
  "Bind a function to the `slb-mode-special-map'.
USAGE: (bind-to-slb-map \"f\" #'full-screen-center)."
  `(define-key slb-mode-special-map (kbd ,key) ,fn))

;; http://emacs.stackexchange.com/a/12906/115
(defun unbind-from-slb-map (key)
  "Unbind a function from the `slb-mode-map'
USAGE: (unbind-from-slb-map \"C-x m f\")
"
  (interactive "kUnset key from slb-mode-map: ")
  (define-key slb-mode-map (kbd (key-description key)) nil)
  (message "%s" (format "Unbound %s key from the %s."
                        (propertize (key-description key)
                                    'face 'font-lock-function-name-face)
                        (propertize "slb-mode-map"
                                    'face 'font-lock-function-name-face))))

(provide 'slb-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; slb-mode.el ends here

