(eval-when-compile
  (require 'cl))

;; XDG Initialization
(defvar xdg-config-home (file-truename (cond ((getenv "XDG_CONFIG_HOME"))
					     (t "~/.config"))))
(defvar xdg-data-home (file-truename (cond ((getenv "XDG_DATA_HOME"))
					   (t "~/.local/share"))))
(defvar xdg-cache-home (file-truename (cond ((getenv "XDG_CACHE_HOME"))
                                            (t "~/.cache"))))

;; Setup emacs directories
(defvar config-home (concat xdg-config-home "/emacs"))
(defvar data-home (concat xdg-data-home "/emacs"))
(defvar cache-home (concat xdg-cache-home "/emacs"))

;; Move custom file
(setq custom-file (concat data-home "/custom"))
(load custom-file :noerror)

;; Load paths
(cl-labels ((add-path (p)
                      (add-to-list 'load-path
                                   (concat config-home "/" p))))
  (add-path "lisp") ;; miscellaneous elisp code
  )

;; autoloads for packages in local lisp directory
(load (concat config-home "/local-autoloads"))

;; Initialization components
(cl-labels ((load-config (c)
                         (load (concat config-home "/config/" c))))
  (load-config "defaults") ;; default variables
  (load-config "packages") ;; standard packages
  (load-config "ibuffer") ;; better buffer management
  (load-config "backups") ;; configure backups
  (load-config "saveplace") ;; restore the point from a previously visited file
  (load-config "title") ;; report the buffer/file name to the terminal
  (load-config "gnupg") ;; configure gpg agent
  (load-config "pianobar") ;; setup pianobar
  (load-config "programming")
  (load-config "folding")
  (load-config "org")
  (load-config "helm")
  (load-config "company")
  (load-config "mouse")
  (load-config "typescript")
  (load-config "projectile")
  (load-config "theme")
  (load-config "linum")
  (load-config "markdown")
  (load-config "omnisharp")
  )

(add-to-list 'slb/packages
             'mustard-theme
             'color-theme-sanityinc-tomorrow)

;; (add-hook 'slb/packages-installed
;;           '(lambda ()
;;              (load-theme 'mustard)))

;;(require 'org-collector)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; fill-column: 75
;; comment-column: 64
;; End:
