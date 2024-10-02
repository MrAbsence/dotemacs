;;Unmark files in the other dired window
(fset 'unmark-other-dired-macro
   (kmacro-lambda-form [?\C-` ?U ?\C-`] 0 "%d"))
(global-set-key (kbd "H-k u") 'unmark-other-dired-macro)

;;Insert start and end time
;; I tried to use kmacro but it usually freezes during running
(defun quick-insert-org-clock-entry ()
  (interactive)
  (org-clock-in)
  (sleep-for 1)
  (org-clock-out)
  )
    
(global-set-key (kbd "H-k c") 'quick-insert-org-clock-entry)

;;Insert today's date
(global-set-key (kbd "H-k d") 'insdate-insert-current-date)

(global-set-key (kbd "H-k p") 'gy/conda-activate-mypycal)

(global-set-key (kbd "H-t") #'gy/org-copy-todo-to-today)
