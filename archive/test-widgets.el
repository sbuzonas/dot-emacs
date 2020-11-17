(require 'widget)

(eval-when-compile
  (require 'wid-edit)
  (require 'xdg-widget))

(defvar widget-example-placeholder)

(defun xdg/test-widgets ()
  "Create the widgets from the XDG package."
  (interactive)
  (switch-to-buffer "*XDG Widgets*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-placeholder)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "XDG Example Widgets.\n\n")
  (widget-create 'xdg-dynamic
                 :tag "Foo"
                 :dynamic-type 'item)
  (widget-insert "\n")
  (widget-insert "XDG Variable Metadata.\n\n")
  (widget-create 'xdg-variable-metadata
                 :value "~"
                 :tag "XDG File Variable"
                 :dynamic-type 'file)
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "Congratulations!")
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (xdg/test-widgets))
                 "Reset Form")
  (widget-insert "\n")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))

(when nil (xdg/test-widgets))
