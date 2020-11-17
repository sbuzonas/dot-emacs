(when (file-executable-p "/usr/local/bin/pianobar")
  (setq pianobar-command "/usr/local/bin/pianobar"))

(defun pianobar-started-p ()
  (interactive)
  (when (boundp 'pianobar-buffer)
    (comint-check-proc pianobar-buffer)))
(autoload 'comint-check-proc "comint")

(defun pianobar-setup-credentials ()
  (interactive)
  (unless (pianobar-started-p)
    (let ((found (nth 0 (auth-source-search :max 1
                                            :host "pandora.com"
                                            :require '(:user :secret :station)))))
      (if found
          (setq pianobar-username (plist-get found :user)
                pianobar-station (plist-get found :station)
                pianobar-password (let ((secret (plist-get found :secret)))
                                    (if (functionp secret)
                                        (funcall secret)
                                      secret))))
      nil)))

(defun slb/pianobar-play-or-pause ()
  (interactive)
  (if (pianobar-started-p)
      (pianobar-pause-song)
    (progn
      (pianobar-setup-credentials)
      (pianobar)
      (pianobar-window-hide))))
(autoload 'pianobar "pianobar"
  "Comint mode for interacting with pianobar for Pandora.com" t)
(autoload 'pianobar-pause-song "pianobar"
  "Pauses or unpauses the pianobar" t)

(define-key global-map [f8] 'slb/pianobar-play-or-pause)
(define-key global-map [f9] 'pianobar-next-song)

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
