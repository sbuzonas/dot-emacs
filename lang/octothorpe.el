;;; octothorpe.el --- configuration for octothorpe -*- lexical-binding: t  -*-

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

(defgroup octothorpe nil
  "Major mode of Octothorpe configuration files."
  :group 'languages)

(defcustom octothorpe-indent-level 2
  "The tab width to use when indenting."
  :type 'integer)

(defconst octothorpe--font-lock-kwds-package
  (list
   (rx buffer-start "%!" (0+ not-newline) eol)

   '(0 font-lock-comment-face))

  "Octothorpe package declaration line.")

(defconst octothorpe-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Quoted Strings
    (modify-syntax-entry ?\" "\"" table)

    ;; Comment Sequence
    (modify-syntax-entry ?\( ". 12" table)
    (modify-syntax-entry ?\) ". 34" table)

    ;; Paren Characters
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\[ ")[" table)

    ;; Prefixes
    ;; (modify-syntax-entry ?\! "'" table)
    ;; (modify-syntax-entry ?\@ "'" table)
    ;; (modify-syntax-entry ?\# "'" table)
    ;; (modify-syntax-entry ?\% "'" table)

    (modify-syntax-entry ?\# "_ p" table)

    table))

;; Indentation
(defun octothorpe--sexp-innermost-char (state) (nth 1 state))
(defun octothorpe--start-of-last-sexp (state) (nth 2 state))
(defun octothorpe--in-string? (state) (nth 3 state))
(defun octothorpe--start-of-string (state) (nth 8 state))

(defun octothorpe--prior-sexp? (state)
  (number-or-marker-p (octothorpe--start-of-last-sexp state)))

(defun octothorpe-indent-function (indent-point state)
  "Indent at INDENT-POINT where STATE is `parse-partial-sexp' for INDENT-POINT."
  (goto-char (octothorpe--sexp-innermost-char state))

  (if (octothorpe--not-resource-form-p)
      (1+ (current-column))
    (forward-char 1)

    (cond ((octothorpe--check-non-symbol-sexp (point))
           (+ 2 (current-column)))

          ((octothorpe--find-indent-spec state)
           (1+ (current-column)))

          (t
           (octothorpe--normal-indent calculate-octothorpe-indent-last-sexp)))))

(define-derived-mode octothorpe-mode prog-mode "Octothorpe"
  :syntax-table octothorpe-mode-syntax-table
  (font-lock-fontify-buffer))

;; Local Variables:
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:

;;; octothorpe.el ends here
