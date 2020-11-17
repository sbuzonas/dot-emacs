;; don't clobber symlinks
(setq backup-by-copying t)

;; use versioned backups
(setq version-control t
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t)

;; Don't backup in place
(defvar backup-dir (concat cache-home "/backups/"))
(setq backup-directory-alist
      `(("." . ,backup-dir)))

;; Same with autosaves
(defvar autosave-dir (concat cache-home "/autosave/"))
(setq auto-save-list-file-prefix autosave-dir
      auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
