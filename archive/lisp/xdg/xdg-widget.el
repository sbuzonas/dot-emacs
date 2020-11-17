;;; xdg-widget.el --- Widgets for configuring the XDG package -*- byte-compile-dynamic: t ; lexical-binding: t -*-
;;
;; Copyright (C) 2016-2018 Steve Buzonas
;;
;; Author: Steve Buzonas <steve@fancyguy.com>
;; Keywords: xdg
;; Version: 0.1.0
;; Package: xdg

;; This file is part of `xdg.el'.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This package provides the widgets necessary for configuring with custom.
;;
;; See `xdg-customize.el' for usage of these widgets.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customize Macros

;; (require 'xdg-customize)

;; Widgets

(define-widget 'xdg-dynamic 'default
  "Base widget that serves as a proxy for dynamic types."
  ;; TODO: Possibly add a robust solution for convert
  :convert-widget 'widget-value-convert-widget
  :default-get 'widget-xdg-surrogate-get
  :doc 'widget-xdg-surrogate-docstring
;;  :basic-format-handler 'widget-basic-format-handler
;;  :extended-format-handler 'widget-extended-format-handler
  :format "%{%t%}: %v"
  :surrogate-fetch 'widget-xdg-dynamic-type
  :match 'widget-xdg-surrogate-match
  :validate 'widget-surrogate-validate
  :value-create 'widget-xdg-surrogate-value-create
  :value-get 'widget-xdg-surrogate-value-get
  :value-inline 'widget-xdg-surrogate-value-inline)

(define-widget 'xdg-variable-metadata 'xdg-dynamic
  "A widget intended for storing data associated to a variable."
  :format "%{%t%}: %v%h"
  :variable '(sexp :tag "Variable"))

(defun xdg-variable-metadata-value-create (widget)
  
  )

;; Format Handlers

(defvar xdg-default-format-handler-map nil
  "Basic char sequence mappings to handlers")

(let ((map (make-sparse-keymap)))
  (define-key map [?%] 'xdg-format-percent)
  (define-key map [?t] 'xdg-format-tag)
  (define-key map [?v] 'xdg-format-value)

  (setq xdg-default-format-handler-map map))

;; (lookup-key xdg-default-format-handler-map [?v])

;; ?\[ button-start
;; ?\] button-end
;; ?\{ sample-begin
;; ?\} sample-end
;; ?b checkbox-button
;; ?d delete-button
;; ?d doc
;; ?h help-text
;; ?i insert-button
;; ?n newline-indent

(defmacro xdg-handle-format (handler widget escape &rest args)
  `(funcall ,handler ,widget ,escape ,@args))

(defvar xdg-character-format-regexp "%\\(.\\)"
  "REGEXP for extracting characters from escape sequences for format processing.")

(defun xdg-format-percent (_widget _escape &rest _opts)
  (insert ?%))

(defun xdg-format-tag (widget _escape &rest _opts)
  (insert (widget-get widget :tag)))

(defun xdg-format-value (widget _escape &rest _opts)
  (widget-apply widget :value-create))

(defun xdg-keymap-format-processor (format-str widget keymap)
  (save-excursion
    (widget-specify-insert
     (let ((from (point))
           (format-args (widget-get widget :format-options))
            to)
       (insert format-str)
       (goto-char from)
       (while (re-search-forward xdg-character-format-regexp nil t)
         (let* ((escape (char-after (match-beginning 1)))
                (handler (lookup-key keymap escape)))
           (delete-char -1)
           (xdg-handle-format handler widget escape format-args)))))))

;; Surrogate Helpers

(defsubst widget-xdg-dynamic-type (widget)
  "Return the :dynamic-type value of WIDGET."
  (widget-get widget :dynamic-type))

(defsubst widget-xdg-surrogate-factory (widget)
  (widget-convert (widget-apply widget :surrogate-fetch)))

(defun widget-xdg-surrogate-get (widget)
  "Get the default value as defined for the surrogate attribute of WIDGET."
  (widget-default-get (widget-xdg-surrogate-factory widget)))

(defun widget-xdg-surrogate-match (widget value)
  "Non-nil if the surrogate value of WIDGET matches VALUE."
  (widget-apply (widget-xdg-surrogate-factory widget) :match value))

(defun widget-xdg-surrogate-validate (widget)
  "The result of validating the surrogate of WIDGET."
  (widget-apply (widget-xdg-surrogate-factory widget) :validate))

(defun widget-xdg-surrogate-value-create (widget)
  "Convert and instantiate the value of the surrogate for WIDGET.
Store the newly created widget in the :surrogate attribute."
  (let* ((value (widget-get widget :value)))
    (widget-put
     widget
     :surrogate (list (widget-create-child-value
                       widget (widget-xdg-surrogate-factory widget) value)))))

(defun widget-xdg-surrogate-value-get (widget)
  "Get the value of the surrogate of WIDGET."
  (widget-value (widget-xdg-surrogate-factory widget)))

(defun widget-xdg-surrogate-value-inline (widget)
  "Get the inline value of the surrogate of WIDGET."
  (widget-apply (widget-xdg-surrogate-factory widget) :value-inline))

(defun widget-xdg-surrogate-docstring (widget value)
  (message "[%s] %s" value widget))

(provide 'xdg-widget)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; comment-column: 75
;; fill-column: 90
;; End:

;;; xdg-widget.el ends here
