(defadvice epg--start (around advice-epg-disable-agent disable)
  "Don't allow epg--start to use gpg-agent in plain text
terminals."
  (if (display-graphic-p)
      ad-do-it
    (let ((agent (getenv "GPG_AGENT_INFO")))
      (setenv "GPG_AGENT_INFO" nil) ; give a text password prompt
      ad-do-it
      (setenv "GPG_AGENT_INFO" agent))))
(ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
(ad-activate 'epg--start)

(defun kludge-gpg-agent ()
  (if (display-graphic-p)
      (setenv "DISPLAY" (terminal-name))
    (setenv "GPG_TTY" (terminal-name))
    (setenv "DISPLAY")))
(add-hook 'window-configuration-change-hook 'kludge-gpg-agent)
;;(add-hook 'after-make-frame-functions 'kludge-gpg-agent)

(require 'auth-source)
(let ((auth-file "~/.authinfo"))
  (when (file-exists-p "~/.authinfo.gpg")
    (setq auth-file "~/.authinfo.gpg"))
  (setq auth-sources `((:source ,auth-file :host t :protocol t))))

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
