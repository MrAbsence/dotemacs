;;Unmark files in the other dired window
(fset 'unmark-other-dired-macro
   (kmacro-lambda-form [?\C-` ?U ?\C-`] 0 "%d"))
(global-set-key (kbd "H-k u") 'unmark-other-dired-macro)

;;Insert start and end time
(fset 'quick-insert-org-clock-entry
   (kmacro-lambda-form [?\C-c ?\C-x tab ?\C-c ?\C-x ?\C-o] 0 "%d"))
(global-set-key (kbd "H-k c") 'quick-insert-org-clock-entry)

;;Insert today's date
(global-set-key (kbd "H-k d") 'insdate-insert-current-date)

(global-set-key (kbd "H-k p") 'gy/conda-activate-mypycal)
