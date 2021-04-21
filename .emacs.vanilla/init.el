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
		            eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq x-stretch-cursor          t         ; stretch cursor to the glyph’s width
      delete-by-moving-to-trash t         ; delete files to trash
      window-combination-resize t         ; take new window space from all other windows
      undo-limit                100000000 ; raise undo limit to 100Mb
      evil-want-fine-undo       t         ; more granular undo with evil
      auto-save-default         t
      truncate-string-ellipsis  "…")

(global-subword-mode 1)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width  . 80))

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
  :defer t)

(use-package ivy
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
  :defer t)

(use-package doom-modeline
  :defer t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :defer t
  :init (load-theme 'doom-nord t))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :defer t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :defer t
  :bind (("M-x"     . counsel-M-x)
	       ("C-x b"   . counsel-ibuffer)
	       ("C-x C-f" . counsel-find-file)
	       :map minibuffer-local-map
	       ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
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
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "t" 'evil-next-visual-line)
  (evil-global-set-key 'motion "s" 'evil-previous-visual-line)
  (define-key evil-normal-state-map "c" nil)
  (define-key evil-normal-state-map "C" nil)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "S" nil)
  (define-key evil-normal-state-map "r" nil)
  (define-key evil-normal-state-map "R" nil)
  (define-key evil-normal-state-map "j" nil)
  (define-key evil-normal-state-map "J" nil)
                                        ;je redéfinis certaines fonctions pour l’état normal
  (define-key evil-normal-state-map "h" 'evil-change)
  (define-key evil-normal-state-map "H" 'evil-change-line)
  (define-key evil-normal-state-map "T" 'evil-join)
  (define-key evil-normal-state-map "l" 'evil-replace)
  (define-key evil-normal-state-map "L" 'evil-replace-state)
  (define-key evil-normal-state-map "k" 'evil-substitute)
  (define-key evil-normal-state-map "K" 'evil-change-whole-line)
                                        ;même chose mais cette fois pour l’état motion
  (define-key evil-motion-state-map "c" 'evil-backward-char)
  (define-key evil-motion-state-map "C" 'evil-window-top)
  (define-key evil-motion-state-map "t" 'evil-next-line)
  (define-key evil-motion-state-map "s" 'evil-previous-line)
  (define-key evil-motion-state-map "r" 'evil-forward-char)
  (define-key evil-motion-state-map "R" 'evil-window-bottom)
  (define-key evil-motion-state-map "j" 'evil-find-char-to)
  (define-key evil-motion-state-map "J" 'evil-find-char-to-backward)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :defer t
  :config
  (evil-collection-init))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("t" text-scale-increase "in")
  ("s" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/code")
    (setq projectile-project-search-path '("~/Documents/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :defer t
  :config (counsel-projectile-mode))

(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
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

;;;;;;;;;;;;;;;; Dashboard

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Phundrak’s Vanilla Emacs"
	dashboard-startup-banner    'logo
	dashboard-center-content    t
	dashboard-show-shortcuts    t
	dashboard-set-navigator     t
	dashboard-set-heading-icons t
	dashboard-set-file-icons    t
	dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
	initial-buffer-choice       (lambda () (get-buffer "*dashboard*")))
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
	    warning))))
  (setq dashboard-items '((recents  . 15)
			  (projects . 10)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))



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

(define-key evil-motion-state-map (kbd ",") nil)

(leader/set-keys
  "b"  "buffers"
  "bb" #'counsel-ibuffer
  "bi" #'ibuffer-list-buffers-and-focus
  "bd" #'kill-this-buffer
  "bD" #'kill-buffer

  "f"  "files"
  "fi" (lambda ()
	 (interactive)
	 (find-file (expand-file-name "init.el" user-emacs-directory)))
  "ff" #'find-file
  "fo" #'phundrak-find-org-files
  "fs" #'save-buffer

  "h"  "help"
  "hdf" #'helpful-callable
  "hdv" #'helpful-variable

  "t"  "toggles"
  "tt" #'counsel-load-theme

  "T"  "text"
  "Ts" #'hydra-text-scale/body

  "w"  "windows"
  "w-" #'split-window-below-and-focus
  "w/" #'split-window-right-and-focus
  "wo" #'other-window
  "wd" #'delete-window
  "wc" #'evil-window-left
  "wt" #'evil-window-down
  "ws" #'evil-window-up
  "wr" #'evil-window-right

  "q"  "quit"
  "qq" #'kill-emacs
  )

(leader/set-keys-for-major-mode 'emacs-lisp-mode
  "e" '("eval" . "evaluate expression")
  "ee" 'eval-last-sexp
  "ed" 'eval-defun
  "er" 'eval-region)

;; Flycheck won’t shut up if I don’t add this
;; (provide 'init)
;;; init.el ends here
