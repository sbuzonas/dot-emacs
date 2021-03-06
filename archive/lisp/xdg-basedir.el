;;; xdg-basedir.el --- Functions and widgets for using XDG compliant paths -*- byte-compile-dynamic: t; lexical-binding: t -*-
;;
;; Copyright (C) 2016-2018 Steve Buzonas
;;
;; Author: Steve Buzonas <steve@fancyguy.com>
;; Package: xdg-basedir

;;; Widget Customization

;;; Widgets

(define-widget 'dynamic 'default
  "A widget that proxies a widget of variable type"
  :format "%{%t%}: %v"
  :convert-widget 'widget-value-convert-widget
  :value-create 'widget-dynamic-value-create
  :value-get 'widget-child-value-get
  :value-inline 'widget-child-value-inline
  :default-get 'widget-dynamic-default-get
  :match 'widget-dynamic-match
  :basic-format-handler 'widget-basic-format-handler
  :extended-format-handler 'widget-extended-format-handler
  :validate 'widget-child-validate)

(defun widget-dynamic-value-create (widget)
  "Convert and instantiate the value of the :dynamic-type attribute of WIDGET.
Store the newly created widget in the :children attribute.

The value of the :dynamic-type attribute should be an unconverted widget type."
  (let ((value (widget-get widget :value))
        (type (widget-get widget :dynamic-type)))
    (widget-put widget :children
                (list (widget-create-child-value widget
                                                 (widget-convert type)
                                                 value)))))

(defun widget-dynamic-default-get (widget)
  "Get the default value according to the :dynamic-type attribute of WIDGET.

The value of the :dynamic-type attribute should be an unconverted widget type."
  (widget-default-get (widget-convert (widget-get widget :dynamic-type))))

(defun widget-dynamic-match (widget value)
  "Non-nil if the value of the :dynamic-type WIDGET matches VALUE.

The value of the :dynamic-type attribute should be an unconverted widget type."
  (widget-apply (widget-convert (widget-get widget :dynamic-type)) :match value))

;;; Helper Functions

(defun xdg/from-env (env &optional default)
  "Returns the value of the specified environment variable or the default if provided"
  (or (getenv env)
      default
      nil))

;;; XDG Customization

(defgroup xdg nil
  "Configure paths to in accordance to the XDG spec."
  :group 'local
  :prefix "xdg-")

(defcustom xdg-enabled t
  "Enable XDG Base Directory supported paths."
  :group 'xdg
  :type 'boolean)

(defgroup xdg-basedir nil
  "Paths defined in the XDG Base Directory specification."
  :group 'xdg)

(defgroup xdg-cache nil
  "Cache specific paths."
  :group 'xdg)

(defgroup xdg-config nil
  "Configuration specific paths."
  :group 'xdg)

(defgroup xdg-data nil
  "Data specific paths."
  :group 'xdg)

(defgroup xdg-runtime nil
  "Runtime specific files."
  :group 'xdg)

(defcustom xdg-cache-home (xdg/from-env "XDG_CACHE_HOME" "~/.cache/")
  "Defines the base directory relative to which user specific non-essential data files should be stored.

  If `$XDG_CACHE_HOME' is unset then a value of `~/.cache/' is used."
  :type 'directory
  :group 'xdg-cache
  :group 'xdg-basedir)

(defcustom xdg-config-home (xdg/from-env "XDG_CONFIG_HOME" "~/.config/")
  "Defines the base directory relative to which user specific configuration files should be stored.

  If `$XDG_CONFIG_HOME' is unset then a value of `~/.config/' is used."
  :type 'directory
  :group 'xdg-config
  :group 'xdg-basedir)

(defcustom xdg-config-dirs (xdg/from-env "XDG_CONFIG_DIRS" "/usr/local/share:/usr/share")
  "Defines the defines the preference-ordered set of base directories to search for configuration files in addition to the `$XDG_CONFIG_HOME' base directory.
  The directories in `$XDG_CONFIG_DIRS' should be seperated with a colon ':'.

  If `$XDG_CONFIG_HOME' is unset then a value of `/usr/local/share:/usr/share' is used."
  :type 'directory
  :group 'xdg-config
  :group 'xdg-basedir)

(defcustom xdg-data-home (xdg/from-env "XDG_DATA_HOME" "~/.data/")
  "Defines the base directory relative to which user specific data files should be stored.

  If `$XDG_DATA_HOME' is unset then a value of `~/.local/share/' is used."
  :type 'directory
  :group 'xdg-data
  :group 'xdg-basedir)

(defcustom xdg-data-dirs (xdg/from-env "XDG_DATA_HOME" "/usr/local/share:/usr/share")
  "Defines the defines the preference-ordered set of base directories to search for data files in addition to the `$XDG_DATA_HOME' base directory.
  The directories in `$XDG_DATA_DIRS' should be seperated with a colon ':'.

  If `XDG_DATA_HOME' is unset then a value of `/usr/local/share:/usr/share' is used."
  :type 'directory
  :group 'xdg-data
  :group 'xdg-basedir)

(defcustom xdg-runtime-dir (xdg/from-env "XDG_RUNTIME_DIR")
  "Defines the base directory relative to which user-specific non-essential runtime files and other file objects (such as sockets, named pipes, ...) should be stored."
  :type 'directory
  :group 'xdg-runtime
  :group 'xdg-basedir)


;;; Wild Wild West

(defvar example-var "~/Projects/"
  "An example variable to be used to showcase dynamic documentation and variable overrides.")

(defcustom example-customization nil
  "Example XDG setting"
  :type '(variable-metadata :variable example-var :dynamic-type string)
  :group 'emacs)

(defvar example-file nil
  "Another demonstration leveraging a file type variable")

(defcustom example-variable "~/.config/.emacs"
  "Example variable widget"
  :tag "Example Variable"
  :type '(variable-metadata :variable example-file :dynamic-type file :tag "Fill")
  :group 'emacs)

(define-widget 'variable-metadata 'dynamic
  "A widget that holds additional data for a variable."
  :format "%{%t%}: %v"
  :field-format "%Ft%n%d"
  :doc-format "%Lv: %h"
  :variable '(sexp :tag "Variable")
  :value-create 'widget-variable-metadata-value-create
  :basic-format-handler 'widget-attribute-format-handler
  :extended-format-handler 'widget-compound-types-format-handler
  :doc 'widget-dynamic-docstring)

(defun widget-specify-indent (widget)
  (and (eq (preceding-char) ?\n)
       (widget-get widget :indent)
       (insert-char ?\s (widget-get widget :indent))))

(defalias 'widget-basic-format-handler 'widget-default-format-handler)
(defun widget-extended-format-handler (widget escape escape-arg)
  (error "Unknown escape `%s'" escape))

(defun widget-attribute-format-handler (widget escape)
  (cond ((eq escape ?n)
         (insert ?\n))
        ((eq escape ?d)
         (widget-insert-documentation (widget)))
        (t (widget-apply widget :format-handler escape))))

(defun widget-compound-types-format-handler (widget escape escape-arg)
  (cond ((eq escape ?F)
         (let ((field (widget-convert (widget-get widget :dynamic-type))))
           (widget-apply widget :basic-format-handler field escape-arg)))
        ((eq escape ?L)
         (let (value type)
           (cond ((eq escape-arg ?i)
                  

         
(widget-member 'variable-metadata :dynamic-type)
(widget-member '(variable-metadata :dynamic-type string) :dynamic-type)
(defun widget-insert-documentation (widget)
  )

(defun widget-variable-metadata-value-create (widget)
  (widget-specify-insert
   (let* ((from (point))
          (children (widget-get widget :children))
          (type (widget-get widget :dynamic-type))
          (variable (widget-get widget :variable))
          (metadata (widget-convert type))
          (field-tag (widget-get metadata :tag))
          (doc-format (widget-get widget :doc-format))
          child)
     (insert (widget-get widget :field-format))
     (goto-char from)
     (while (re-search-forward "%\\(%\|[A-Z].\|.\\)" nil t)
       (let* ((escape (char-after (match-beginning 1)))
              (match-size (string-width escape))
              (escape-arg (char-before (match-beginning 1)))
              child)
         (delete-char (- match-size))
         (cond ((eq escape ?%)
                (insert ?%))
               (t (and (cond
                        ((eq match-size 1)
                         (setq child (widget-apply
                                      widget
                                      :basic-format-handler widget escape)))
                        ((eq match-size 2)
                         (setq child (widget-apply
                                      widget
                                      :extended-format-handler widget escape escape-arg))))
                       (setq children (cons child children)))))
         (widget-specify-indent)))
     (widget-put widget :children children))))
                                     
    (widget-create-child-and-convert widget type
                                     :tag field-tag)))

    ;; (widget-create-child-and-convert widget type
    ;;                                  :tag field-tag)))

    ;; (message "type: %s" type)
    ;; (message "variable: %s" variable)
    ;; (message "metadata: %s" metadata)
    ;; (message "var-format: %s" var-format)
    ;; (widget-create-child-and-convert widget 'variable-item
    ;;                                  :value variable
    ;;                                  :format var-format)))
                                     
        ;;  (child (widget-convert type))
        ;; (indent (widget-get widget :indent))
        ;; (shown (widget-get (widget-get widget :parent) :documentation-shown))
    ;; (start (point)))
    (widget-apply metadata :value-create)))
;;    (widget-create-child-value widget type var)))
    ;; (widget-create-child-and-convert
    ;;  widget 'variable-item
    ;;  :value var)))

;; (defun widget-color-value-create (widget)
;;   (widget-field-value-create widget)
;;   (widget-insert " ")
;;   (widget-create-child-and-convert
;;    widget 'push-button
;;    :tag " Choose " :action 'widget-color--choose-action)
;;   (widget-insert " "))


(defun widget-dynamic-docstring (widget value)
  (message "[%s] %s" value widget))

;; (defcustom var-metadata nil
;;   "Variable metadata test."
;;   :group 'emacs
;;   :type '(variable-metadata :variable config-home :dynamic-type string))

;; (defvar some-var-to-replace "hello")

;; (defcustom var-sample "sample"
;;   "Variable sample test."
;;   :group 'emacs
;;   :type '(variable-metadata :variable some-var-to-replace :dynamic-type string))

;; (defun widget-dynamic-docstring (widget &optional value)
;;   "Get the documentation string specified by the :dynamic-type WIDGET, or nil if none.

;; The value of the :dynamic-type attribute should be an unconverted widget type."
;;   (message "[%s] %s" value widget)
;;   nil)

;; (defun widget-dynamic-format-handler (widget escape)
;;   (let* ((type (widget-get widget :dynamic-type))
;;          (next (widget-convert type))
;;   (widget-apply (widget-convert ) :format-handler escape))


(provide 'xdg-basedir)
