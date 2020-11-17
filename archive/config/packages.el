(eval-when-compile (require 'cl-lib))

(require 'package)
(setq package-user-dir (concat data-home "/packages"))

(setq package-archives
      '(("elpa"         . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("org"          . 10)
        ("elpa"         . 5)
        ("melpa"        . 0)))

(defvar slb/packages '(editorconfig
                       js2-mode
                       mustard-theme
                       npm-mode
                       yaml-mode))

(defun slb/append-selected-packages ()
  (setq package-selected-packages (append package-selected-packages slb/packages)))

(defun slb/install-packages ()
  "Ensure the packages I use are installed. See `slb/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p slb/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (message "%s" slb/packages)
      (message "%s" missing-packages)
      (mapc #'package-install missing-packages)
      (run-hooks 'slb/missing-packages-installed))
    (run-hooks 'slb/packages-installed)))
(package-initialize)
(add-hook 'after-init-hook 'slb/append-selected-packages)
(add-hook 'after-init-hook 'slb/install-packages)

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
