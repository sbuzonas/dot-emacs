;; -*- lexical-binding: t  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't clobber symlinks
(setq backup-by-copying t)

;; Use central location for backups/autosaves
(defvar backup-dir (concat cache-home "/backups/"))
(setq backup-directory-alist `(("." . ,backup-dir)))

(defvar autosave-dir (concat cache-home "/autosave/"))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq auto-save-list-file-prefix (concat autosave-dir ".saves-"))

;; Version control
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq vc-make-backup-files t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:
