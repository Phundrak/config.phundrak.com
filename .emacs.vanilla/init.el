;;; init.el --- My vanilla Emacs configuration -*- lexical-binding: t -*-

;; Author: Lucien Cartier-Tilet
;; Maintainer: Lucien Cartier-Tilet
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://labs.phundrak.com/emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(setq user-full-name       "Lucien Cartier-Tilet"
      user-real-login-name "Lucien Cartier-Tilet"
      user-login-name      "phundrak"
      user-mail-address    "lucien@phundrak.com")

(setq epa-pinentry-mode 'loopback)

(defvar phundrak/default-font-size 90
  "Default font size.")

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq visible-bell t) ; set up visible bell

(setq display-time-format "%Y-%m-%d %H:%M")
(display-time-mode 1) ; display time in modeline

;; Display battery in modeline when using a laptop
(unless (equal "Battery status not available"
	       (battery))
  (display-battery-mode 1))

(set-face-attribute 'default nil :font "Cascadia Code" :height phundrak/default-font-size)


(setq frame-title-format
      '(""
	"%b"
	(:eval
	 (let ((project-name (projectile-project-name)))
	   (unless (string= "-" project-name)
	     (format (if (buffer-modified-p) " ◉ %s" "  ●  %s") project-name))))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escabe-quit)

;; Answer with y or n, not yes or not
(defalias 'yes-or-no-p 'y-or-n-p)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		special-mode-hook
		helpful-mode-hook
		woman-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq x-stretch-cursor          t         ; stretch cursor to the glyph’s width
      delete-by-moving-to-trash t         ; delete files to trash
      window-combination-resize t         ; take new window space from all other windows
      undo-limit                100000000 ; raise undo limit to 100Mb
      evil-want-fine-undo       t         ; more granular undo with evil
      auto-save-default         t
      truncate-string-ellipsis  "…")

(global-subword-mode 1)

(setq-default major-mode 'org-mode)

(defun modeline-contitional-buffer-encoding ()
  "Hide \"LF UTF-8\" in modeline.

It is expected of files to be encoded with LF UTF-8, so only show
the encoding in the modeline if the encoding is worth notifying
the user."
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'modeline-contitional-buffer-encoding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Packages              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode
  :straight (:build t)
  :defer t)

(use-package ivy
  :straight (:build t)
  :defer t
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-t" . ivy-next-line)
	 ("C-s" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-t" . ivy-next-line)
	 ("C-s" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-t" . ivy-next-line)
	 ("C-s" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; NOTE: Thi first time you load your configuration on a new machine,
;; you’ll need to run the following command interactively o that mode
;; line icons display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons
  :straight (:build t)
  :defer t)

(use-package doom-modeline
  :straight (:build t)
  :defer t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :straight (:build t)
  :defer t
  :init (load-theme 'doom-nord t))

(use-package rainbow-delimiters
  :straight (:build t)
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :straight (:build t)
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :straight (:build t)
  :defer t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :straight (:build t)
  :defer t
  :bind (("M-x"     . counsel-M-x)
	 ("C-x b"   . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :straight (:build t)
  :defer t
  :custom
  (counsel-describe-function-function #'helpfull-callable)
  (counsel-describe-variable-function #'helpfull-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package bind-map
  :straight (:build t)
  :ensure t
  :defer )

(use-package spaceleader
  :defer t
  :after bind-map
  :straight (spaceleader :type git
			 :host github
			 :repo "mohkale/spaceleader")
  :config
  (setq leader-key "SPC"
	leader-major-mode-prefix ","))

(use-package evil
  :straight (:build t)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "t" 'evil-next-visual-line)
  (evil-global-set-key 'motion "s" 'evil-previous-visual-line)

  (define-key evil-normal-state-map "c" nil)
  (define-key evil-normal-state-map "C" nil)
  (define-key evil-normal-state-map "t" nil)
  (define-key evil-normal-state-map "T" nil)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "S" nil)
  (define-key evil-normal-state-map "r" nil)
  (define-key evil-normal-state-map "R" nil)
  (define-key evil-normal-state-map "h" nil)
  (define-key evil-normal-state-map "H" nil)
  (define-key evil-normal-state-map "j" nil)
  (define-key evil-normal-state-map "J" nil)
  (define-key evil-normal-state-map "k" nil)
  (define-key evil-normal-state-map "K" nil)
  (define-key evil-normal-state-map "l" nil)
  (define-key evil-normal-state-map "L" nil)

  (define-key evil-motion-state-map "h" 'evil-replace)
  (define-key evil-motion-state-map "H" 'evil-replace-state)
  (define-key evil-motion-state-map "j" 'evil-find-char-to)
  (define-key evil-motion-state-map "J" 'evil-find-char-to-backward)
  (define-key evil-motion-state-map "k" 'evil-substitute)
  (define-key evil-motion-state-map "K" 'evil-smart-doc-lookup)
  (define-key evil-motion-state-map "l" 'evil-change)
  (define-key evil-motion-state-map "L" 'evil-change-line)

  (define-key evil-motion-state-map "c" 'evil-backward-char)
  (define-key evil-motion-state-map "C" 'evil-window-top)
  (define-key evil-motion-state-map "t" 'evil-next-line)
  (define-key evil-motion-state-map "T" 'evil-join)
  (define-key evil-motion-state-map "s" 'evil-previous-line)
  (define-key evil-motion-state-map "S" 'evil-lookup)
  (define-key evil-motion-state-map "r" 'evil-forward-char)
  (define-key evil-motion-state-map "R" 'evil-window-bottom)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :straight (:build t)
  :defer t
  :config
  (evil-collection-init))

(use-package hydra
  :straight (:build t)
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("t" text-scale-increase "in")
  ("s" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(use-package projectile
  :straight (:build t)
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!

  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :straight (:build t)
  :defer t
  :config (counsel-projectile-mode))

(use-package magit
  :straight (:build t)
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :straight (:build t)
  :defer t)

(defun phundrak-find-org-files ()
  "Find all org files in the directories listed in
`phundrak-org-directories', then list them in an ido buffer where
the user can match one and open it."
  (interactive)
  (find-file
   (ivy-completing-read
    "Org File: "
    (s-split "\n"
             (mapconcat (lambda (path)
                          (shell-command-to-string
                           (format "fd . %s -e org -c never" path)))
                        phundrak-org-directories
                        "\n")))))

(use-package mixed-pitch
  :defer t
  :straight t
  :hook
  (org-mode . mixed-pitch-mode))

;;;;;;;;;;;;;;;; Dashboard

(use-package dashboard
  :straight (:build t)
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Phundrak’s Vanilla Emacs"
	dashboard-startup-banner    'logo
	dashboard-center-content    t
	dashboard-show-shortcuts    t
	dashboard-set-navigator     t
	dashboard-set-heading-icons t
	dashboard-set-file-icons    t
	initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
	dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-navigator-buttons
	`(
	  ((,(all-the-icons-faicon "language" :height 1.1 :v-adjust 0.0)
	    "Linguistics website"
	    ""
	    (lambda (&rest _) (browse-url "https://langue.phundrak.com")))

	   (,(all-the-icons-faicon "firefox" :height 1.1 :v-adjust 0.0)
	    "Config Website"
	    ""
	    (lambda (&rest _) (browse-url "https://config.phundrak.com"))))

	  ((,(all-the-icons-octicon "git-branch" :height 1.1 :v-adjust 0.0)
	    "Dotfiles sources"
	    ""
	    (lambda (&rest _) (browse-url "https://labs.phundrak.com/phundrak/dotfiles")))
	   ("!" "Issues" "Show issues" (lambda (&rest _)
					 (browse-url "https://labs.phundrak.com/phundrak/dotfiles/issues"))
	    warning))
	  ((,(all-the-icons-faicon "level-up" :height 1.1 :v-adjust 0.0)
	    "Update packages"
	    ""
	    (lambda (&rest _) (progn
				(require 'straight)
				(straight-pull-all)))))))

  (setq dashboard-items '((recents  . 15)
			  (projects . 10)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

(use-package vterm
  :defer t
  :straight t
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Email

(use-package mu4e
  :straight (:build t :location site)
  :commands mu4e mu4e-compose-new
  :init
  (provide 'html2text)
  (when (or (not (require 'mu4e-meta nil t))
	    (version< mu4e-mu-version "1.4"))
    (setq mu4e-maidir                 "~/.mail"
	  mu4e-trash-folder           "/Trash"
	  mu4e-refile-folder          "/Archive"
	  mu4e-sent-folder            "/Sent"
	  mu4e-drafts-folder          "/Drafts"
	  mu4e-user-mail-address-list nil))
  (setq mu4e-attachment-dir
	(lambda (&rest _)
	  (expand-file-name ".attachments" (mu4e-roo-maildir))))
  :config
  (setq mu4e-get-mail-command         "mbsync -a"
	mu4e-update-interval          60
	mu4e-compose-format-flowed    t
	mu4e-view-show-addresses      t
	mu4e-sent-messages-behaviour  'sent
	mu4e-hide-index-messages      t
	;; try to show images
	mu4e-view-show-images         t
	mu4e-view-image-max-width     600
	;; configuration for sending mail
	message-send-mail-function    #'smtpmail-send-it
	smtpmail-stream-type          'starttls
	message-kill-buffer-on-exit   t                  ; close after sending
	;; start with the first (default) context
	mu4e-context-policy           'pick-first
	;; compose with the current context, or ask
	mu4e-compose-context-policy   'ask-if-none
	;; use ivy
	mu4e-completing-read-function #'ivy-completing-read
	;; no need to ask
	mu4e-confirm-quit             t
	mu4e-header-fields
	'((:account    . 12)
	  (:human-date . 12)
	  (:flags      . 4)
	  (:from       . 25)
	  (:subject)))

  ;; set mail user agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Use fancy icons
  (setq mu4e-use-fancy-chars     t
	mu4e-headers-draft-mark     `("D" . ,(all-the-icons-faicon "pencil":height 0.8))
	mu4e-headers-flagged-mark   `("F" . ,(all-the-icons-faicon "flag":height 0.8))
	mu4e-headers-new-mark       `("N" . ,(all-the-icons-faicon "rss":height 0.8))
	mu4e-headers-passed-mark    `("P" . ,(all-the-icons-faicon "check":height 0.8))
	mu4e-headers-replied-mark   `("R" . ,(all-the-icons-faicon "reply":height 0.8))
	mu4e-headers-seen-mark      `("S" . ,(all-the-icons-faicon "eye":height 0.8))
	mu4e-headers-unread-mark    `("u" . ,(all-the-icons-faicon "eye-slash":height 0.8))
	mu4e-headers-trashed-mark   `("T" . ,(all-the-icons-faicon "trash":height 0.8))
	mu4e-headers-attach-mark    `("a" . ,(all-the-icons-faicon "paperclip":height 0.8))
        mu4e-headers-encrypted-mark `("x" . ,(all-the-icons-faicon "lock":height 0.8))
        mu4e-headers-signed-mark    `("s" . ,(all-the-icons-faicon "certificate":height 0.8)))

  (setq mu4e-bookmarks
        `((,(s-join " "
                    '("NOT flag:trashed"
                      "AND (maildir:/Inbox OR maildir:/Junk)"
                      "AND NOT to:CONLANG@LISTSERV.BROWN.EDU"
                      "AND NOT to:AUXLANG@LISTSERV.BROWN.EDU"
                      "AND NOT to:ateliers-emacs@framalistes.org"
                      "AND NOT to:ateliers-paris@emacs-doctor.com"
                      "AND NOT list:ateliers-emacs.framalistes.org"
                      "AND NOT list:ateliers-paris.emacs-doctor.com"))
           "Inbox" ?i) ;; Inbox without the linguistics mailing lists
          (,(s-join " "
                    '("NOT flag:trashed"
                      "AND (maildir:/Inbox OR maildir:/Junk)"
                      "AND (f:/.*up8\.edu|.*univ-paris8.*/"
                      "OR c:/.*up8\.edu|.*univ-paris8.*/"
                      "OR t:/.*up8\.edu|.*univ-paris8.*/)"))
           "University" ?u) ;; University-related emails
          (,(s-join " "
                    '("to:CONLANG@LISTSERV.BROWN.EDU"
                      "OR to:AUXLANG@LISTSERV.BROWN.EDU"))
           "Linguistics" ?l) ;; linguistics mailing lists
          (,(s-join " "
                    '("list:ateliers-emacs.framalistes.org"
                      "OR to:ateliers-paris@emacs-doctor.com"
                      "OR list:ateliers-paris.emacs-doctor.com"))
           "Emacs" ?e) ;; Emacs mailing list
          ("maildir:/Sent" "Sent messages" ?s)
          ("flag:unread AND NOT flag:trashed" "Unread messages" ?U)
          ("date:today..now AND NOT flag:trashed" "Today's messages" ?t)
          ("date:7d..now AND NOT flag:trashed" "Last 7 days" ?w)
          ("date:1m..now AND NOT flag:trashed" "Last month" ?m)
          ("date:1y..now AND NOT flag:trashed" "Last year" ?y)
          ("flag:trashed AND NOT flag:trashed" "Trash" ?T)
          ("mime:image/* AND NOT flag:trashed" "Messages with images" ?p)))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))

(use-package org-msg
  :straight (:build t)
  :hook (mu4e-compose-pre . org-msg-mode)
  :config
  (setq org-msg-startup              "inlineimages"
	org-msg-greeting-name-limit  3
	org-msg-default-alternatives '(html text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org

(use-package org-appear
  :after org
  :straight (:build t)
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis   t
	org-appear-autolinks      t
	org-appear-autosubmarkers t
	org-appear-autoemphasis   t)
  (run-at-time nil nil #'org-appear--set-elements)
  :defer t)

(use-package org-superstar
  :after org
  :straight (:build t)
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet   ?\s
	org-superstar-leading-fallback ?\s
	org-hide-leading-stars         nil
	org-superstar-todo-bullet-alist
	'(("TODO" . 9744)
	  ("[ ]"  . 9744)
	  ("DONE" . 9745)
	  ("[X]"  . 9745))))


(use-package org-fancy-priorities
  :straight (:build t)
  :hook (org-mode        . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list `(,(all-the-icons-faicon "flag"     :height 1.1 :v-adjust 0.0)
				    ,(all-the-icons-faicon "arrow-up" :height 1.1 :v-adjust 0.0)
				    ,(all-the-icons-faicon "square"   :height 1.1 :v-adjust 0.0))))

(use-package evil-nerd-commenter
  :straight (:build t)
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             Keybindings             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar phundrak-org-directories '("~/org"
                                   "~/Documents/university/S8"
                                   "~/Documents/conlanging")
  "Directories in which to look for org files with the function
`phundrak-find-org-files'.")

(defun split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun ibuffer-list-buffers-and-focus ()
  (interactive)
  (ibuffer-list-buffers)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun eshell-new ()
  "Open a new instance of Eshell."
  (interactive)
  (eshell 'N))

(define-key evil-motion-state-map (kbd ",") nil)

(leader/set-keys
  "SPC" #'counsel-M-x

  "a"   "apps"
  "as"  "shells"
  "ase" #'eshell-new
  "asv" #'vterm

  "b"   "buffers"
  "bb"  #'counsel-ibuffer
  "bi"  #'ibuffer-list-buffers-and-focus
  "bd"  #'kill-this-buffer
  "bD"  #'kill-buffer
  "bh"  #'dashboard-refresh-buffer
  "bs"  (lambda ()
	  (interactive)
	  (switch-to-buffer "*scratch*"))

  "c"   "code"
  "cl"  #'evilnc-comment-or-uncomment-lines

  "e"   "emails"
  "em"  #'mu4e
  "ec"  #'mu4e-compose-new

  "f"   "files"
  "fi"  (lambda ()
          (interactive)
          (find-file (expand-file-name "init.el" user-emacs-directory)))
  "ff"  #'find-file
  "fo"  #'phundrak-find-org-files
  "fs"  #'save-buffer

  "h"   "help"
  "hk"  #'which-key-show-top-level
  "hdf" #'helpful-callable
  "hdk" #'helpful-key
  "hdv" #'helpful-variable

  "t"   "toggles"
  "tt"  #'counsel-load-theme

  "T"   "text"
  "Ts"  #'hydra-text-scale/body

  "w"   "windows"
  "w-"  #'split-window-below-and-focus
  "w/"  #'split-window-right-and-focus
  "wo"  #'other-window
  "wd"  #'delete-window
  "wD"  #'delete-other-windows

  "wc"  #'evil-window-left
  "wt"  #'evil-window-down
  "ws"  #'evil-window-up
  "wr"  #'evil-window-right

  "q"   "quit"
  "qf"  #'delete-frame
  "qq"  #'save-buffers-kill-terminal
  "qQ"  #'kill-emacs)

(leader/set-keys-for-major-mode 'emacs-lisp-mode
  "e" '("eval" . "evaluate expression")
  "ee" 'eval-last-sexp
  "ed" 'eval-defun
  "er" 'eval-region)

;; Flycheck won’t shut up if I don’t add this
;; (provide 'init)
;;; init.el ends here
