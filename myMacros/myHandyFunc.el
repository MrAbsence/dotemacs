;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; Personal settings
;; -----------------------------------------------------------------------------
(defun gy/restore-my-nine-tabs ()
  "If some error happens, the desktop file may not work. Thus, need to restore my tabs."
  (interactive)
  (let ((tab-names '("EditNote" "EditCode" "EditFile" "Programming" "Agenda"
                     "TODOIBuffer" "Dired-Files" "Debug"))) ; List of tab names
    (dotimes (i (length tab-names)) ; Create and rename tabs based on list length
      (tab-bar-new-tab)                     ; Create a new tab
      (tab-bar-rename-tab (nth i tab-names)))) ; Rename tab with corresponding name
  )


;; TODO, then switch projects?
(defun gy/conda-activate-mypycal ()
  "conda activate path to mypycal"
  (interactive)
  (conda-env-activate-path (expand-file-name "~/mypycal"))
  )

;; -----------------------------------------------------------------------------
;; Org mode, agenda, task related
;; -----------------------------------------------------------------------------

;; Delete some unnecessary columns in clock table
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
;; From system crafters
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; The buffer you put this code in must have lexical-binding set to t!
;; See the final configuration at the end for more details.

(defun gy/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun gy/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (gy/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun gy/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (delete-dups (append org-agenda-files (gy/org-roam-list-notes-by-tag "Project")))))
;; I add delete duplicates here to ensure it does not add files more than once, if you run it more than once in Emacs

(defun gy/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'gy/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun gy/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'gy/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (gy/org-roam-filter-by-tag "Project")
   nil
   :templates
   '(("p" "project" plain (function (lambda () (gy/file-to-string-by-filename (expand-file-name "MyNotes/Templates/ProjectTemplate.org" gy-dropbox-location))))
      :if-new (file+head "@Inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: Project\n#+filetags: Project\n#+date: %U\n\n")
      :unnarrowed t))))

;; This key binding is set in init.el Org-roam section
;; (global-set-key (kbd "C-c n p") #'gy/org-roam-find-project)


(defun gy/org-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!

        (org-capture-templates
         '(
           ("j" "Journal Entries")
      ;; Usually, the journal is for current time.
      ;; But if you want to journal for a different time (e.g., tomorrow), use "C-1 C-C j ...."!!!
           ("jt" "Today Task" plain
            (file+olp+datetree (lambda () (gy/capture-add-week-to-journalname (expand-file-name "MyNotes/90-DayPlanner/" gy-dropbox-location))))
           ""
           :immediate-finish t
           :tree-type week
           )
           ))

        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-capture-goto-target "jt")
      (setq today-file (gy/capture-add-week-to-journalname (expand-file-name "MyNotes/90-DayPlanner/" gy-dropbox-location)))
      (setq pos (point))
      ;;(print pos) ;;For debug
      )

    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-refile nil (list "TodayTasks" today-file nil pos) nil)
        (org-refile nil nil (list "TodayTasks" today-file nil pos)))
    ))

;; -----------------------------------------------------------------------------
;; Emacs, buffer basic functions
;; -----------------------------------------------------------------------------


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

(defun gy/file-to-string-by-filename (filename)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun gy/buffer-whole-string-by-buffername (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring (point-min) (point-max)))))

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
