(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("roam notes"
      (and
       (used-mode . org-mode)
       (name . "^20")))
     ("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
	(starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(package-selected-packages
   '(magit counsel-projectile projectile bind-key dired-preview erc xref idlwave eglot eldoc faceup flymake jsonrpc org project soap-client tramp verilog-mode elfeed python-mode seq org-inline-anim deadgrep wolfram-mode org-contrib conda compat yasnippet all-the-icons-dired emacsql org-roam ace-window visual-fill-column org-bullets helpful counsel which-key rainbow-delimiters doom-themes all-the-icons doom-modeline ivy use-package))
 '(safe-local-variable-values
   '((eval my-buffer-local-set-key
	   (kbd "M-<up>")
	   'outline-move-subtree-up)
     (eval my-buffer-local-set-key
	   (kbd "M-<down>")
	   'outline-move-subtree-down)
     (eval my-buffer-local-set-key
	   (kbd "C-c C-c")
	   'outline-hide-entry)
     (eval when
	   (fboundp 'gy/gyinit-setup)
	   (gy/gyinit-setup)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
