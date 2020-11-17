(require 'mwheel)
(require 'mouse)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1) ; keyboard scroll one line at a time

(xterm-mouse-mode 1)
(mouse-wheel-mode 1)

(global-set-key [mouse-4] 'previous-line)
(global-set-key [mouse-5] 'next-line)

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
