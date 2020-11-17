;;; javascript.el --- configuration for javascript -*- lexical-binding: t  -*-

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

(define-skeleton js-license
  "Inserts a license section for javascript files"
  ""
  "/**\n"
  " * Copyright (c) " (format-time-string "%Y") "-present, " organization "\n"
  " * All rights reserved.\n"
  " *\n"
  " * Licensed under the Apache License, Version 2.0 (the \"License\");\n"
  " * you may not use this file except in compliance with the License.\n"
  " * You may obtain a copy of the License at\n"
  " *\n"
  " *     http://www.apache.org/licenses/LICENSE-2.0\n"
  " *\n"
  " * Unless required by applicable law or agreed to in writing, software\n"
  " * distributed under the License is distributed on an \"AS IS\" BASIS,\n"
  " * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n"
  " * See the License for the specific language governing permissions and\n"
  " * limitations under the License.\n"
  " */\n")

(define-skeleton js-header
  "Inserts a header for a javascript file"
  ""
  '(setq v1 (skeleton-read "Executable? "))
  '(setq shebang (if (string= v1 "y") "#!/usr/bin/env node\n" ""))
  '(setq v2 (skeleton-read "Strict? "))
  '(setq strict (if (string= v2 "y") "'use strict';\n" ""))
  shebang
  '(js-license)
  strict)

(defun slb/use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :bind (:map js2-mode-map
              ("C-c i h" . js-header))
  :config
  (add-hook 'js2-mode-hook #'slb/use-js-executables-from-node-modules))

(use-package indium
  :hook ((js-mode js2-mode) . indium-interaction-mode))

(use-package rjsx-mode)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; javascript.el ends here
