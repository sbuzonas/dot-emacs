(dolist (item '(markdown-mode markdown-mode+ markdown-toc))
  (add-to-list 'slb/packages item))

(defun slb/markdown-init ()
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(if (package-installed-p 'markdown-mode)
    (slb/markdown-init)
  (add-hook 'slb/packages-installed 'slb/markdown-init))
