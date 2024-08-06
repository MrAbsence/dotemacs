;;; init.el --- Mrabsence's emacs config
;;; Cool things start here!----------------------------------------
;;; Thanks to https://www.youtube.com/@SystemCrafters
;;; The major part of code is from his introducing video
;;; This is a project with .projectile

;;★0.0 Speed up Emacs startup
;;The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;★0.1 Define system/computer specific varibles
;;★★ Directories variables
;; Please use $HOME based directory and dropbox based dir as the main work place
(defconst gy-dropbox-location "/mnt/c/Users/MrAbsence/Dropbox/")
(defconst gy-anaconda-home "/home/mrabsence/anaconda3/")

;;★★ Load my functions/packages
(add-to-list 'load-path "~/.emacs.d/MrabsencePackages/mathematica")
;; my ob-mathematica.el package!!
;; now it can run code blocks

;; I want to load some kbd macros
(setq my-macros-file (locate-user-emacs-file "myMacros/myMacros.el"))
(load my-macros-file 'noerror 'nomessage)

;; I also want to load some handy functions
(setq my-handy-function-file (locate-user-emacs-file "myMacros/myHandyFunc.el"))
(load my-handy-function-file 'noerror 'nomessage)

;;★★ Windows, MSYS2 specific settings
;; on Windows, more specifically, MSYS2 system,
;; Previously, I need to use windows python but not msys2 python for eaf package
;; msys2 cannot install pyqt6 and pyqt6-PyQtWebEngine, but windows python can do it
;; conda python cannot install pyqt6 and pyqt6-PyQtWebEngine, but it can install the Qt 5 version

;; I noticed the proxy cannot update packages in emacs
;; So reset it here.
(setenv "http_proxy")
(setenv "https_proxy")

;;★★ Windows, WSL settings
;; Emacs on WSL open links in Windows web browser
;; Teach Emacs how to open links in your default Windows browser

(let ((wsl-cmd-exe "/mnt/c/Windows/System32/cmd.exe")
	(wsl-cmd-c-args '("/c" "start" "")))
    (when (file-exists-p wsl-cmd-exe)
      (setq browse-url-generic-program  wsl-cmd-exe
	    browse-url-generic-args     wsl-cmd-c-args
	    browse-url-browser-function 'browse-url-generic
	    search-web-default-browser 'browse-url-generic)))


;;★1 Some basic settings of editor

;;★★1.1 Display or not display settings
;;★★★ Disable some not important messages/toolbar
;; prevent showing startup page-----------------------------------
;; and other useless widgets--------------------------------------

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;;★★★ Display time at modeline
(setq display-time-day-and-date nil) ; Do not display the date
(setq display-time-default-load-average nil) ; Do not display cpu load
(display-time-mode 1)       ; Display the time at modeline


;;★★★ Tab bar display and its hot keys
;; enable tabbar and make it look good
;; need to define the modifier first to let tab-bar-mode process it
(setq tab-bar-select-tab-modifiers '(hyper))
(setq tab-bar-show t)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))

;; Now I can use ``H-TabNumber'' to switch tabs
;; In reality, press `f6' then the number
;; `f6-then-0' recent tab
;; `f6-then-9' last tab
(setq tab-bar-tab-hints t)
(define-key function-key-map (kbd "<f6>") 'event-apply-hyper-modifier)
;;the C-tab switch tab is defined in tab bar mode automatically
;;as long as we enable tab bar mode

;;this function does not work here? fixed.
(tab-bar-mode 1)
(global-set-key (kbd "H-t") 'tab-bar-mode)

;;★★★ ibuffer and its display format
;;----------------------------------------------------------------
;; Use ibuffer to manage buffers----------------------------------
;;----------------------------------------------------------------
;; Set up ibuffer display format
;; Make buffer name fully displayed
(global-set-key (kbd "<f12>") 'ibuffer)
(setq ibuffer-formats
  '((mark modified read-only locked " "
       (name 55 55 :left :elide)
       " "
       (size 9 -1 :right)
       " "
       (mode 18 18 :left :elide)
       " " filename-and-process)
    (mark " "
       (name 16 -1)
       " " filename)))

;;★★★ Desktop save mode! (save what is displayed)
;;----------------------------------------------------------------
;;Save your previous window, buffer and tag settings--------------
;;bind some keys to switch windows/tabs---------------------------
;;----------------------------------------------------------------
(desktop-save-mode 1)

;;★★1.2 Set-keys (global-set-key)
;;★★★ Switch between windows
;;Bye! Windows short cut!
;;Now M-tab is work for Emacs
;;(w32-register-hot-key [M-tab])
;;It is not necessary!

;; If there are many windows, just use C-x o, which invoke ace window
;; It is not necessary to use C-~ to do (other-window 1)
(global-set-key (kbd "C-`") (lambda () (interactive) (other-window -1)))

;;★★★ Completion key setting
;;I want the completion all the time
;;this means <f6> then i
;;TODO: get hint when I type in 3 or 4 letters
(global-set-key (kbd "H-i") 'completion-at-point)

;;★★★ Evil keys (TODO)
;;----------------------------------------------------------------
;; Reserved for evil key bindings---------------------------------
;; I may need to switch to it one day------------------------------
;;----------------------------------------------------------------
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;;★★1.3 General editor settings

;;★★★ Encoding
;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;;★★★ Cursor, scroll and its movement
;; change the cursor shape
;; and make it blink forever
(setq-default cursor-type '(bar . 6))
(setq-default blink-cursor-blinks 0)
;; When I just move my cursor line by line, do not make it jump.
(setq scroll-conservatively 51)
;;(set-cursor-color "#4aa832") ;;The theme overwrite it.

(pixel-scroll-precision-mode 1)

;; mouse color
(add-to-list 'default-frame-alist '(mouse-color . "white"))

;;★★★ Auto save back-up files
;;set auto save interval in seconds
(setq-default auto-save-visited-interval 0)

;;★★★ History of files, minibuffer (shell) command, cursor locations
;;Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Save what you enter into minibuffer prompts
;; and shell...
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of
;;**opened files**
(save-place-mode 1)

;;★★★ Real-time reverting buffer
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;★★★ Show line numbers and column numbers
;; show line numbers and col numbers in some modes
(column-number-mode)
;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;★★★ default font and font size
;; This line works well in Win32
(set-face-attribute 'default nil :family "Consolas" :height 155)

;; set fonts for fixed pitch and varible pitch
(set-face-attribute 'fixed-pitch nil
                    :font "Consolas"
                    :weight 'regular
                    :height 155)

(set-face-attribute 'variable-pitch nil
		    :font "Cantarell"
		    :weight 'regular
		    :height 155)

;;★★★ How does Emacs window looks like (see doom theme package)
;; General emacs views--------------------------------------------
;; Now I mainly use Doom theme------------------------------------

;; set frame size and position
;; WHO WANT A SMALL WINDOW?
;;(when (display-graphic-p)
;;      (set-frame-size (selected-frame) 59 36)
;;      (set-frame-position (selected-frame) 1200 55))

;;old display section
;; I tried to setup colors
;; but it is not necessary in theme
;; Now, I use it to adjust client frames
;;(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
;;(add-to-list 'default-frame-alist '(background-color . "#620b9c"))
;;(add-to-list 'default-frame-alist '(width  . 60))
;;(add-to-list 'default-frame-alist '(height . 36))
;;(add-to-list 'default-frame-alist '(left . 1200))
;;(add-to-list 'default-frame-alist '(top . 55))

;;this is the theme
;;(load-theme 'wombat)
;;I decide to use doom theme instead

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))


;;★2 Packages
;;★★2.1 Built-in packages
;;★★★ upgrade built-in
(setq package-install-upgrade-built-in t)
;;I updated compat package from ELPA

;;★★★ Warnings
;;Warning suppression
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))


;;★★★ TRAMP
;; My remote files
;;set up ssh in ssh/config
(defun gy/connect-remote ()
  (interactive)
  (dired "/sshx:mrab1:~/"))

;;C-x C-f /sshx:mrab|sudo::/path/to/file
;;for debug tramp
;;(setq tramp-default-remote-shell "/bin/bash")
;;(setq tramp-verbose 9)
;;★★★ TRAMP Bug fix
;;Finally, I solved this by NOT activate conda at bash startup!
;;BUT to activate it in Emacs
;;Conda is NOT the system python!


;;★★★ Dired
;;----------------------------------------------------------------
;;Set up dired
;;----------------------------------------------------------------
(setq dired-use-ls-dired t
      dired-dwim-target t
      dired-listing-switches "-l --all --group-directories-first --human-readable --no-group --time-style=+%D")
;; Use "(" to hide/show the details of dired
;; Dired related packages are in Packages


;;★★★ webjump
(require 'webjump)
(add-to-list 'webjump-sites
             '("Google" .
               [simple-query "www.google.com"
                             "www.google.com/search?q=" ""]))


;;★★2.2 Basic package settings
;;★★★ Package sources and packages initialize
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ("elpanongnu" . "https://elpa.nongnu.org/nongnu/")))

;;★★★ gnupg key bug fix
;; this line will not work on linux/iOS system
;; For msys2 only

;;★★★ Package initialize
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;;★★★ install use-package on non-Linux platforms

;;I heard this is finally in Emacs 29
;;Here it goes.
;;(unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;★★★ Custom variable file
;; Move customization variables to a separate file and load it
;; the custom-vars.el is in the same dir as init.el
;; !!!This is important!!!
;; Copy the custom file with your setting files
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;★★2.3 Learn Emacs Packages
;;★★★ Command-log-mode
;;For learning Emacs, you can turn command-log-mode ON
;;You need to run the following command to log your steps
;;1. M-x global-command-log-mode
;;2. C-c o (clm/toggle-command-log-buffer)
;;(use-package command-log-mode
;;  :demand
;;  :config
;;  (global-command-log-mode 1))

;;★★★ Which-key (displaying key bindings and function names)
;; Auto completion functions
;; and help system in emacs
;; a good tool to learn basic key bindings in Emacs
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))


;;★★2.4 Theme Packages

;;★★★ Doom theme: themes, modeline
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom")
  ;; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; custom modeline
;; It should be good in Emacs 28 and 29 now
;; It uses nerd-icons to display the icons on the modeline
(use-package doom-modeline
 :ensure t
 :init (doom-modeline-mode 1)
 :custom ((doom-modeline-height 30)
	  (doom-modeline-time-icon nil)
	  (doom-modeline-workspace-name nil))
 
)

;;★★★ All-the-icons: Dired
;; all-the-icons used to provide the icons on doom mode line
;; now nerd-icons does the work
;; all-the-icons is still needed for all-the-icons-dired
(use-package all-the-icons
  :ensure t)
;; Use good looking icons in dired!
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;;★★2.5 Functional Packages

;;★★★ Rainbow-delimiters
;; Help you to find the right pairs of delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;★★★ Ace-window (for switching windows)
;; ace window is good at dealing more than two windows
(use-package ace-window
  :demand)
(global-set-key (kbd "C-x o") 'ace-window)

;;★★★ Counsel and Ivy (completion)
;; counsel
;; It was loaded in ivy. Here is some config
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; Use ivy for completion
;; How to run ivy?
;; demand makes it run at start
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-backward)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
	 ("C-l" . ivy-alt-done)
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :demand
  :config
  (ivy-mode 1))

;;★★★ Helpful (a very good help document system)
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;;★★★ YAsnippet (Yet Another snippet)

;;(global-unset-key (kbd "<apps>")) ;;historically, <menu> is <apps>
(global-unset-key (kbd "<menu>"))
(use-package yasnippet
  :demand
  :bind
  (:map yas-minor-mode-map
        ("<menu>" . yas-expand) ;; now <menu> key do the work
	("<f9>" . yas-expand) ;; in case you don't have <menu> key
        ([(tab)] . nil) ;; tab nolonger works for yasnippet
        ("TAB" . nil)
   :map yas-keymap
	([(tab)] . nil)
	("TAB" . nil)
	([(shift tab)] . nil)
	([backtab] . nil)
	("<menu>" . yas-next-field)
	("<f9>" . yas-next-field)
	("S-<menu>" . yas-prev-field)
	("S-<f9>" . yas-prev-field))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/mySnippets"))
  ;; So this directory should be part of my config
  (yas-global-mode 1))

;;★★★ Deadgrep
;; Let us do full text search in Emacs
;; It works OK.
;; TODO: But I need to investigate it to write some more convienent functions, e.g. only search notes
(use-package deadgrep
 :after org-roam
 :demand
 :config
 (global-set-key (kbd "<f5>") #'deadgrep)
 )

;;★★★ Projectile and Magit
;;----------------------------------------------------------------
;;Use magit to do version control
;;Projectile for project
;;Set it up later.
;;----------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p (expand-file-name "MyNotes/10-Projects/Coding" gy-dropbox-location))
    (setq projectile-project-search-path (list (expand-file-name "MyNotes/10-Projects/Coding" gy-dropbox-location))))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;★★★ Elfeed
(use-package elfeed
  :ensure t
  :bind ("H-x e" . elfeed)
  :config
  (setq elfeed-feeds
        '(("https://semiengineering.com/feed/" magzine semiconduct)
	  	  ))
  ;;(setq-default elfeed-search-filter "@2-week-ago +unread ")
  )

;; (defun gy/elfeed-mark-all-as-read ()
;;   (interactive)
;;   (mark-whole-buffer)
;;   (elfeed-search-untag-all-unread))

;; ;; Optional: Bind a key for marking all as read
;; (define-key elfeed-search-mode-map (kbd "R") 'gy/elfeed-mark-all-as-read)

;;★★★ Dired-preview

(use-package dired-preview
  :ensure t
  :bind
  (:map dired-mode-map
        ("H-p" . dired-preview-mode) ;; <f6> then p
	)
  :config
  (setq dired-preview-delay 0.5)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
      (concat "\\."
              "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
              "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
              "\\|iso\\|epub\\|pdf\\)"))
  )

;;★★2.6 Programming Languages
;;★★★ Python and Conda 
;; Decide to switch to virtualenv (Later)
;;!Set up Conda and Python

;;The default is python3 but conda does not have a python3
(setq python-shell-interpreter "python")


(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home gy-anaconda-home)
  (setq conda-env-home-directory gy-anaconda-home)
  ;; (conda-env-activate "base")
  ;; activate conda base at start
  ;; If you do not want to activate base at start, just comment it out.
  )
  ;; If this command start emacs in the "base" environment
  ;; Thus, all the org babel-python sections are running in conda "base"
  ;; (setq org-babel-python-command "python")
  ;; (setq python-shell-interpreter "python")
  ;; if you really need to run the system python (MSYS2 python)
  ;; change to (setq python-shell-interpreter "python3") !! because conda does not have python3 command. (need to confirm)


;;★★2.7 Org Mode Configuration

;;★★★ Some setting functions
(defun gy/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


;;org font set up
(defun gy/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; Note: they inherit from outline-X faces (see outline mode section)
  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.12)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;;★★★ This is org-mode
(use-package org
  :hook (org-mode . gy/org-mode-setup)
  :custom
  ;;control the log content in agenda
  (org-agenda-start-with-log-mode '(clock closed))
  ;;where to display the new agenda window
  (org-agenda-window-setup 'current-window)
  ;;day view by default
  (org-agenda-span 'day)
  ;;show habits in all days
  (org-habit-show-habits-only-for-today nil)
  ;;set image size inline
  (org-image-actual-width '(250))
  ;;make the todo selection in minibuffer
  (org-use-fast-todo-selection 'expert)
  :config
  (gy/org-font-setup)

  ;; make org mode pretty
  ;; make the collapse sign available for bold font
  (setq org-ellipsis "⤵"
	org-hide-emphasis-markers t)
  ;;for super/supscript display or export
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts "{}")

  ;;This is a specific highlight for me!
  (delete '("~" org-code verbatim) org-emphasis-alist)
  (add-to-list 'org-emphasis-alist
             '("~" (:foreground "red" :weight bold)
               ))
  
  ;;using default apps to open links
  ;;add other files if it is necessary
  (setq org-file-apps
     '(("\\.docx?\\'" . default)
       ("\\.xlsx?\\'" . default)
       ("\\.pptx?\\'" . default)
      ("\\.mm\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default)
      (auto-mode . emacs)
      (directory . emacs)))
  
  ;;define a 0-width character in different system
  ;;it is helpful if you want to use superscript at the start of line/word, e.g. zwsp^{13}C
  (add-to-list 'org-entities-user
      '("zwsp"
      "\\hspace{0pt}"       ; latex
      nil             ; not in math-mode
      "&#8203;"           ; html
      ""              ; ascii
      nil                 ; latin1 not sure what to put here
      "​"             ; utf-8
      ))
  
  ;;set org latex preview options
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))

  ;;set org log content
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;;org-cite
  ;;(require 'org-cite)
  ;;already included in org mode 9.5
  (setq org-cite-global-bibliography (list (expand-file-name "MyNotes/91-generalInfo/updatedMyLibrary.bib" gy-dropbox-location)))
  ;;TODO: Can I directly follow the citation to its notes?


  ;;Auto complete
  ;;e.g., <s ==> script
  ;;<q ==> quote
  (require 'org-tempo)
  )


;;★★★ Org-agenda and TODO options
  ;;org-agenda
  ;;Use the dropbox as a reference point for all the useful files.
  (setq org-agenda-files
	(list (expand-file-name "MyNotes/80-GTDTasks/MyTasks.org" gy-dropbox-location)
	  (expand-file-name "MyNotes/40-Archives/TaskArchive.org" gy-dropbox-location)
	  (expand-file-name "MyNotes/80-GTDTasks/HabitsOrLongTasks.org" gy-dropbox-location)
	  (expand-file-name "MyNotes/80-GTDTasks/MyReadingList.org" gy-dropbox-location)))
  

  ;;track my habits
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 30)

  ;;TODO: modify keywords and their colors
  (setq org-todo-keywords
     ;;active and deactive is for repeated tasks/plans
    '((sequence "ACTIVE(a)" "|" "ONE(z)" "DEACTIVE(e@)") ;;use this for repeat tasks/habits
      (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")  ;; small or subtasks
      (sequence "WAIT(w@/!)" "HOLD(h@)" "WANTTODO(o)" "PLAN(p)" "READY(r)" "CURRENT(u)" "REVIEW(v)" "|" "COMPLETED(c!)" "NOGOOD(g@)" "CANCELED(k@)"))) ;;big projects

  (setq org-todo-keyword-faces
      '(("WANTTODO" . (:foreground "GoldenRod" :weight bold))
        ("PLAN" . (:foreground "IndianRed1" :weight bold))
        ("TODO" . (:foreground "IndianRed1" :weight bold))
        ("WAIT" . (:foreground "coral" :weight bold))
        ("READY" . (:foreground "OrangeRed" :weight bold))
	("NEXT" . (:foreground "OrangeRed" :weight bold))
        ("CURRENT" . (:foreground "LimeGreen" :weight bold))
        ("REVIEW" . (:foreground "LimeGreen" :weight bold))
	("NOGOOD" . (:foreground "red" :weight bold))
	))

  ;; refile tasks
  ;; maybe add some refile files for quick notes?
  ;; BTW, I learned how to use cons in this section!
  (setq org-refile-targets
    (list (cons (expand-file-name "MyNotes/40-Archives/TaskArchive.org" gy-dropbox-location) (cons :maxlevel 1)) ; this is task archive
	  (cons (expand-file-name "MyNotes/80-GTDTasks/MyTasks.org" gy-dropbox-location) (cons :maxlevel 1))))
  ;; !!in case the task was created somewhere else. Use M-w to copy task.

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; Also save org files after remote edits
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)

  ;; Create my own tags
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@personal" . ?P)
       ("@home" . ?H)
       ("work" . ?W)
       ("read" . ?r)
       ("write" . ?w)
       ("publish" . ?p)
       ("experiment" . ?e)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  ;; Should rewrite this someday
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 12)))
      (todo "NEXT|READY"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "work/!-HOLD" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT|READY"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "WANTTODO"
            ((org-agenda-overriding-header "Want To Do")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "CURRENT"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED|NOGOOD"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANCELED"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

;;★★★ Org-capture templates
  ;; Org capture templates
  (setq org-capture-templates
    '(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp (lambda () (expand-file-name "MyNotes/80-GTDTasks/MyTasks.org" gy-dropbox-location)) "Task inbox")
       "* PLAN %?\n  :PROPERTIES:\n :CREATED: %U\n :END:\n %i\n  %a\n\n" :empty-lines 1)
      ;; Task line + Properties created time + selected region + link

      ("j" "Journal Entries")
      ;; Usually, the journal is for current time.
      ;; But if you want to journal for a different time (e.g., tomorrow), use "C-1 C-C j ...."!!!
      ("jj" "Journal" entry
           (file+olp+datetree (lambda () (gy/capture-add-week-to-journalname (expand-file-name "MyNotes/90-DayPlanner/" gy-dropbox-location))))
           "\n* %<%I:%M %p> - %? :journal:\n\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree (lambda () (gy/capture-add-week-to-journalname (expand-file-name "MyNotes/90-DayPlanner/" gy-dropbox-location))))
           "\n* %<%I:%M %p> - %^{prompt|GroupMeeting} :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ("ja" "Agenda Output" entry
           (file+olp+datetree (lambda () (gy/capture-add-week-to-journalname (expand-file-name "MyNotes/90-DayPlanner/" gy-dropbox-location))))
           "\n* %<%I:%M %p> - Org Agenda :journal:\n\n#+begin_src\n%(gy/buffer-whole-string-by-buffername \"*Org Agenda*\")\n#+end_src\n%?\n"
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree  (lambda () (gy/capture-add-week-to-journalname (expand-file-name "MyNotes/90-DayPlanner/" gy-dropbox-location))))
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline (lambda () (expand-file-name "MyNotes/90-DayPlanner/Metrics.org" gy-dropbox-location)) "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
  ;;So link to org can do text search
  (setq org-link-search-must-match-exact-headline nil)

  (define-key global-map (kbd "C-c j")
   (lambda () (interactive) (org-capture nil nil)))

;;★★★ Org image settings
  ;; show the figure at this point
  (define-key org-mode-map (kbd "C-c v") #'gy/org-display-inline-image-at-point)

  ;; shortcut for figure directory
  (setq org-link-abbrev-alist
	(list (cons "figdir" (expand-file-name "MyNotes/99-figures/" gy-dropbox-location))
	      (cons "notedir" (expand-file-name "MyNotes/" gy-dropbox-location))
	      (cons "wprojects" (expand-file-name "MyNotes/10-Projects/Work-M/" gy-dropbox-location)) ;; this path will only work on my work laptop!!! Should be edited based on the work path
	      ))

;;★★★ Org-babel and mathematica
;; My favorate Prog languages ------------------------------------
;; and Org-babel -------------------------------------------------
;;----------------------------------------------------------------
;; Load mathematica from contrib

(use-package org-contrib
  :after org
  )

(use-package wolfram-mode
  )

(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(setq wolfram-program "math")
(add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))
(add-to-list 'auto-mode-alist '("\\.wl$" . wolfram-mode))
(setq wolfram-path (expand-file-name "MyNotes/30-Resources/WolframCodes" gy-dropbox-location)) ;; e.g. on Linux ~/.Mathematica/Applications

;;org-babel
(org-babel-do-load-languages
   'org-babel-load-languages
   '(
       ;;(awk . t)
     (calc .t)
     (C . t)
     (emacs-lisp . t)
       ;; (haskell . t)
     (gnuplot . t)
       ;; (latex . t)
       ;; (ledger . t)
       ;; (js . t)
       ;; (http . t)
       ;; (perl . t)
     (python . t)
       ;; (php . t) ;; org-babel does not currently support php.  That is really sad.
       ;; (R . t)
       ;; (scheme . t)
     (shell . t)
     (sql . t)
     ;;(sqlite . t)
     (mathematica . t) ;; Oh, my god!
     ))

  ;; No need for confirm each run
  (setq org-confirm-babel-evaluate nil)
  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; Wolfram Mathematica
;; Sanitize output and deal with paths
;; (setq org-babel-mathematica-command "mash")
;; Font-locking
(add-to-list 'org-src-lang-modes '("mathematica" . wolfram))
;; For wolfram-mode (but this is not defined?)
;; (setq mathematica-command-line "mash")


;;★★★ Other org packages

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  )

(use-package org-inline-anim
  :after org
  :hook (org-mode . org-inline-anim-mode)
  )

(defun gy/org-mode-visual-fill ()
  (setq visual-fill-column-width 130
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
;;let org mode display in the center of the screen
(use-package visual-fill-column
  :hook (org-mode . gy/org-mode-visual-fill))

;;★★★ Org-roam Note!!

;;Use org roam to write notes
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ;;suppress warning
  :custom
  (org-roam-directory (file-truename (expand-file-name "MyNotes" gy-dropbox-location)))
  (org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:12}" 'face 'org-tag)))

  (org-roam-completion-everywhere t)

  ;; Org roam capture templates
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "@Inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("c" "chemical" plain (function (lambda () (gy/file-to-string-by-filename (expand-file-name "MyNotes/Templates/ChemicalTemplate.org" gy-dropbox-location))))
      :if-new (file+head "30-Resources/ChemistryCompoundsInfo/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Compound\n\n")
      :unnarrowed t)

     ("e" "english" plain
      "%?"
      :if-new (file+head "30-Resources/English(Terms)/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n* Meaning \n\n* Examples \n\n")
      :unnarrowed t)
     ;;Finally got the template function work!
     ("p" "project" plain (function (lambda () (gy/file-to-string-by-filename (expand-file-name "MyNotes/Templates/ProjectTemplate.org" gy-dropbox-location))))
      :if-new (file+head "@Inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project\n#+date: %U\n\n")
      :unnarrowed t)

     ("l" "literature" plain (function (lambda () (gy/file-to-string-by-filename (expand-file-name "MyNotes/Templates/LiteratureTemplate.org" gy-dropbox-location))))
      :if-new (file+head "30-Resources/!Literatures!/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Literature\n\n")
      :unnarrowed t)

     ("c" "source code" plain (function (lambda () (gy/file-to-string-by-filename (expand-file-name "MyNotes/Templates/SourceCodeTemplate.org" gy-dropbox-location))))
      :if-new (file+head "30-Resources/Linux-Programming-Knowledge/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: SourceCode\n\n")
      :unnarrowed t)
     ;;TODO: add book here. add experiment here.
     ))
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  ;;demand here is very important
  :demand
  :config
  ;;(setq org-roam-database-connector 'sqlite3)
  (setq org-id-extra-files (org-roam--list-files org-roam-directory))
  (org-roam-db-autosync-mode))

;;Control the display of org roam buffer
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.35)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

;;★★2.8 Old or not-used or not-working functions/packages


;;★★★ rescan org roam
;; These three functions are used for rescan all org roam notes
;; Edited based on https://www.eigenbahn.com/2021/09/15/org-roam
;; But I found them are not very useful. ...
;;(defun gy/file-ext-org-p (file-full-name)
;;  (and (f-ext-p file-full-name "org")
;;  (not (backup-file-name-p (f-filename file-full-name)))))

;;(defun gy/org-roam/add-index-current ()
;;  "Add index to file of currently visited buffer, if applicable."
;;  (interactive)
;;  (unless (and (buffer-file-name)
;;		       (file-exists-p (buffer-file-name)))
;;    (user-error "Current buffer is not visiting a file that exists on disk."))

;;  (unless (gy/file-ext-org-p (buffer-file-name))
;;    (user-error "Current buffer is not visiting an indexable file."))

;;  (unless (org-id-get)
;;    (org-id-get-create)
;;    (call-interactively #'save-buffer))
;;  (org-id-update-id-locations (list (buffer-file-name)))
;;  (org-roam-db-update-file))

;;(defun gy/org/index-rescan-all ()
;;  "Populate `org-id-locations' by rescaning recursively all files in `org-roam-directory'."
;;  (interactive)
;;  (let ((buffs-snapshot (buffer-list)))
;;    (org-id-update-id-locations
;;     (f-files org-roam-directory #'gy/file-ext-org-p t))
    ;; NB: `org-id-update-id-locations' opens all matching files, we close them after processing
;;    (mapc #'kill-buffer
;;          (-difference (buffer-list) buffs-snapshot))))

;;(defun gy/org-roam/rescan ()
;;  "Force rescan of whole `org-roam-directory'."
;;  (interactive)
;; (gy/org/index-rescan-all)
;;  (org-roam-db-autosync-mode 1))

;;★★★ EAF and Clash
;;Set up EAF
; (add-to-list 'load-path "~/.emacs.d/MrabsencePackages/emacs-application-framework/")
; (require 'eaf)
;   (setq eaf-browser-continue-where-left-off t)
;   (setq eaf-browser-enable-adblocker t)
;   (setq browse-url-browser-function 'eaf-open-browser)
;   (setq eaf-mindmap-dark-mode nil) ; default option
;   (setq eaf-browser-dark-mode "force")
;   (setq eaf-terminal-dark-mode nil)
;   (setq eaf-pdf-dark-mode nil)

;   (defalias 'browse-web #'eaf-open-browser)
;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;   ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;   ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind delete_cookie, see more in the Wiki

; (require 'eaf-browser)
; (require 'eaf-pdf-viewer)
; (require 'eaf-rss-reader)
; (require 'eaf-all-the-icons)
; (require 'eaf-jupyter)

; (start-process-shell-command "clash-prxy" "*clash-1*" "clash -f ~/my/clash-config.yaml")
; (setq eaf-proxy-type "http")
; (setq eaf-proxy-host "127.0.0.1")
; (setq eaf-proxy-port "18080")

;;★★2.9 Emacs start up settings

;;★★★ Start something at startup
;;Make org-agenda ready at start up
(add-hook 'after-init-hook 'org-agenda-list)

;; Now we have an Emacs server to connect. Cool!
(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
   (server-start))

;;★★★ Measure startup time
(defun gy/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done)
  )

(add-hook 'emacs-startup-hook #'gy/display-startup-time)

;;★★★ Force recompile packages. (DEBUG only)!
;;(byte-recompile-directory package-user-dir nil 'force)

;;★★★ Clean up
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 2 1000 1000))

;;★Local setting for init.el
;;★★The cool outline mode
(require 'outline)

(dolist (face '((outline-1 . 1.3)
                  (outline-2 . 1.2)
                  (outline-3 . 1.1)
                  (outline-4 . 1.0)
                  (outline-5 . 0.9)
                  (outline-6 . 0.9)
                  (outline-7 . 0.9)
                  (outline-8 . 0.9)))
      (set-face-attribute (car face) nil :font "Ubuntu Mono" :weight 'regular :height (cdr face)))

(defun gy/gyinit-setup ()
  (interactive)
  (when (equal (expand-file-name "~/.emacs.d/init.el")
               (buffer-file-name (current-buffer)))
    (setq-local outline-regexp ";;[\u2605]+")
    (setq-local outline-heading-alist '((";;★" . 1) (";;★★" . 2) (";;★★★" . 3)))
    ;;The button only works in Emacs 29. Should consider to install it some day.
    ;;Here it comes.
    (setq-local outline-minor-mode-use-buttons 't)
    (setq-local outline-minor-mode-highlight 'override)
    (setq-local outline-minor-mode-cycle t)
    (setq-local outline-level 'outline-level)
    (set-display-table-slot standard-display-table 
                         'selective-display 
                         (string-to-vector "˅..."))
    (outline-minor-mode)
    )
  )

;; Need to confirm the local varibles for the first start
;; Local Variables:
;; eval: (when (fboundp 'gy/gyinit-setup) (gy/gyinit-setup))
;; eval: (my-buffer-local-set-key (kbd "C-c C-c") 'outline-hide-entry)
;; eval: (my-buffer-local-set-key (kbd "M-<down>") 'outline-move-subtree-down)
;; eval: (my-buffer-local-set-key (kbd "M-<up>") 'outline-move-subtree-up)
;; End:

;;; init.el ends here
