(defun slb/org-tangle-included (&optional target-file lang)
  (interactive)
  (save-mark-and-excursion
   (save-restriction
     (widen)
     (org-org-export-as-org nil)
     (with-current-buffer "*Org ORG Export*"
       (org-babel-tangle nil target-file lang)))))

(defun slb/org-tangle-included-file (file &optional target-file lang)
  (interactive "fFile to tangle: \nP")
  (save-mark-and-excursion
   (with-current-buffer (find-file file)
     (slb/org-tangle-included target-file lang))))

;; (let* ((tmp-file (make-temp-file "org-tangle-with-include"))
;;        (org-export-preprocess-after-include-files-hook
;; 	`((lambda ()
;; 	    (message "In preprocess-hook for %s" ,tmp-file)
;; 	    (let ((s (buffer-string)))
;; 	      (with-temp-file ,tmp-file (insert s)))))))
;;   (save-window-excursion (org-html-export-as-html))
;;   (switch-to-buffer
;;    (get-buffer-create "*Org ORG Tangle*"))
;;   (insert-file-contents tmp-file))
;; (org-mode))

;; (defun ded/org-export-as-org-to-buffer ()
;;   (interactive)
;;   (let* ((tmp-file (make-temp-file "org-tangle-with-include"))
;; 	 (org-export-preprocess-after-include-files-hook
;; 	  `((lambda () (let ((s (buffer-string)))
;; 			 (with-temp-file ,tmp-file (insert s)))))))
;;     (save-window-excursion (org-export-as-html-to-buffer nil))
;;     (switch-to-buffer
;;      (get-buffer-create "*Org Org Export*"))
;;     (insert-file-contents tmp-file))
;;   (org-mode))
  
;; (defun ded/tangle-with-include-files ()
;;   (interactive)
;;   (save-window-excursion
;;     (ded/org-export-as-org-to-buffer)
;;     (org-babel-tangle)))

;; (defvar endless/init.org-message-depth 3
;;   "What depth of init.org headers to message at startup.")

;; (with-temp-buffer
;;   (insert-file "~/.emacs.d/init.org")
;;   (goto-char (point-min))
;;   (search-forward "\n* init.el")
;;   (while (not (eobp))
;;     (forward-line 1)
;;     (cond
;;      ;; Report Headers
;;      ((looking-at
;;        (format "\\*\\{2,%s\\} +.*$"
;;                endless/init.org-message-depth))
;;       (message "%s" (match-string 0)))
;;      ;; Evaluate Code Blocks
;;      ((looking-at "^#\\+BEGIN_SRC +emacs-lisp.*$")
;;       (let ((l (match-end 0)))
;;         (search-forward "\n#+END_SRC")
;;         (eval-region l (match-beginning 0))))
;;      ;; Finish on the next level-1 header
;;      ((looking-at "^\\* ")
;;       (goto-char (point-max))))))
