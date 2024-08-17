(defun gy/restore-my-nine-tabs ()
  "If some error happens, the desktop file may not work. Thus, need to restore my tabs."
  (interactive)
  (let ((tab-names '("EditNote" "EditCode" "EditFile" "Programming" "Agenda"
                     "TODOIBuffer" "Dired-Files" "Debug"))) ; List of tab names
    (dotimes (i (length tab-names)) ; Create and rename tabs based on list length
      (tab-bar-new-tab)                     ; Create a new tab
      (tab-bar-rename-tab (nth i tab-names)))) ; Rename tab with corresponding name
  )

;; As the name delete this buffer's file
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))



(defun gy/journal-clocktable-write (&rest args)
  "Custom clocktable writer.
   Uses the default writer but shifts the first column right."
  (apply #'org-clocktable-write-default args)
  (save-excursion
    (forward-char) ;; move into the first table field
    (org-table-next-field)
    (org-table-next-field)
    (org-table-delete-column)
    ))

(defun insdate-insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
     With a prefix argument, the date is inserted without the day of
     the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))

(defun my-buffer-local-set-key (key command)
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

;; generate file name for week journal
;; example: 2022-0913-W33.org. The date is the Monday of the week.
;; DONE: it works with C-1 org-capture
;; DONE: it works with org-agenda-capture!
;; When org-agenda-capture overrides default-time (5th parameter of org-read-date)
;; or org-capture receives "C-1",
;; Force to open a prompt to receive the Monday date
(defun gy/capture-add-week-to-journalname (path)
  (concat path
    (format-time-string "%Y-%m%d-W%V"
      (if (or org-overriding-default-time (equal current-prefix-arg 1))
	(org-read-date nil t nil "Could you select the Monday for filename: ")
	(org-read-date nil t "++1" nil (org-read-date nil t "-Sun")))
      nil) "-WeekJournal.org"))

(defun gy/buffer-whole-string-by-buffername (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring (point-min) (point-max)))))

;; display inline image right at point
;; one image at a time
;; Still neet to use C-c C-x C-v to turn it off
(defun gy/org-display-inline-image-at-point ()
  (interactive)
  (let* ((context (org-element-context (org-element-at-point)))
         (type (org-element-type context))
         (beg  (plist-get (cadr context) :begin))
         (end  (plist-get (cadr context) :end)))
     (when (eq type 'link)
        (org-display-inline-images nil nil beg end))))


(defun gy/add-roamref-to-literature-node (roam-node)
  ;;org roam supports both org-cite and org-ref
  ;;For now, I will mainly use org-cite (included in org mode 9.5)
  (org-roam-ref-add (concat "@" (org-roam-node-title roam-node)))
  ;;Some tips for literature notes are included here.
  (concat "Please note: " "Key concept; Spectial methods; Key results and discussions"))
;;TO MY SURPRISE, IT WORKS!!!

;; Bind this to C-c n I in org-roam settings
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun gy/file-to-string-by-filename (filename)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; TODO, then switch projects?
(defun gy/conda-activate-mypycal ()
  "conda activate path to mypycal"
  (interactive)
  (conda-env-activate-path (expand-file-name "~/mypycal"))
  )
