;; -*- lexical-binding: t  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modular declarations
(defvar modules-dir (concat config-home "/modules/")
  "Path to modules.")

(defvar submodule-path) ;; dynamic variable

(defsubst module-path (name)
  (file-truename modules-dir path))

(defmacro activate-module (name)
  (when 
  (load (module-path path)))

(activate-module "defaults")
(activate-module "packages")
(activate-module "libraries")
(activate-module "theme")
(activate-module "backups")
(activate-module "history")
(activate-module "windows")
(activate-module "completion")
(activate-module "helm")
(activate-module "programming")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:
