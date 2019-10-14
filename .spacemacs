;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(ansible
     asm
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      :disabled-for
                      org
                      git
                      github)
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c-mode
            c-c++-adopt-subprojects t
            c-c++-enable-clang-support t
            c-c++-enable-google-style t
            c-c++-enable-c++11 t)
     (cmake :variables
            cmake-enable-cmake-ide-support t)
     conlanging
     csv
     colors
     dired-phundrak
     django
     docker
     emacs-lisp
     epub
     ess
     git
     github
		 graphviz
     (go :variables
				 go-use-gometalinter t
				 go-tab-width 2)
     gnus
     gpu
     (helm :variables
           helm-no-header t
           helm-use-fuzzy 'source)
     (html :variables
           web-fmt-tool 'web-beautify)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     imenu-list
     (javascript :variables
                 javascript-backend 'tern
                 javascript-fmt-tool 'web-beautify)
     (json :variables
           json-fmt-tool 'web-beautify)
     (latex :variables
            latex-build-command "xelatex"
            latex-enable-auto-fill t
            latex-enable-folding t
            latex-enable-magic t)
     (markdown :variables
							 markdown-live-preview-engine 'vmd
               markdown-mmm-auto-modes '("c"
                                         "c++"
                                         "python"
                                         "rust"
                                         ("elisp" "emacs-lisp")))
     nginx
     (org :variables
          org-enable-reveal-js-support t
          org-enable-github-support t
          spaceline-org-clock-p t
          org-enable-sticky-header t
          org-enable-epub-support t
          org-projectile-file "TODOs.org"
          org-download-image-dir "~/Pictures/org"
          org-return-follows-link t)
     pdf
     (plantuml :variables
               plantuml-jar-path "/opt/plantuml/plantuml.jar"
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
     prettier
     prolog
     (python :variables
             python-backend 'anaconda
             python-sort-imports-on-save t
             python-fill-column 80
             python-enable-yapf-format-on-save t)
     (restclient :variables
                 restclient-use-org t)
     rust
     scheme
     semantic
     (shell :variables
            shell-default-height 40
            shell-default-position 'right
            shell-default-shell 'eshell)
     shell-scripts
     selectric
     semantic
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     enable-flyspell-auto-completion nil)
     (syntax-checking :variables
                      spell-checking-enable-by-default nil
                      spell-checking-enable-auto-dictionary t
                      syntax-checking-enable-tooltips t
                      syntax-checking-use-original-bitmaps t)
     systemd
     (treemacs :variables
               treemacs-use-follow-mode nil
               treemacs-use-filewatch-mode t)
     twitter
     unicode-fonts
     w3m
     web-beautify
     xkcd
     web-beautify
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(atomic-chrome
																			cmake-mode
                                      dionysos
                                      doom-themes
                                      edit-indirect
                                      elcord
                                      evil
                                      eshell-git-prompt
                                      fireplace
                                      kaolin-themes
                                      meson-mode
                                      modern-cpp-font-lock
                                      multiple-cursors
                                      outorg
                                      pinentry
                                      visual-fill-column
                                      wttrin
                                      xresources-theme
                                      yasnippet-snippets)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory nil

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 15)
                                (projects . 15))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(xresources
                         doom-vibrant
                         doom-nord
                         spacemacs-dark
                         doom-one
                         doom-opera
                         doom-dracula
                         doom-molokai
                         doom-peacock
                         doom-sourcerer
                         doom-spacegrey
                         kaolin-dark
                         kaolin-aurora
                         kaolin-bubblegum
                         kaolin-galaxy
                         kaolin-mono-dark
                         kaolin-temple
                         kaolin-valley-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom
                                  :separator wave
                                  :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 8.0
                               :weight normal
                               :width normal)
   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   dotspacemacs-line-numbers '(:relative nil
                                         :enabled-for-modes prog-mode)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%b (%m)"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (load "~/.emacs.d/private/private_emacs.el")
  (require 'org-id)
  (require 'package)
  (require 'ox-latex)
  (require 'ox-publish)

  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://melpa.milkbox.net/packages/")
                           ("org" . "https://orgmode.org/elpa/")))

  ;; (add-to-list 'package-archives
  ;; '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (flyspell-mode 0)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Media                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq dionysos-backend 'mpd
        dionysos-notify-p t
        dionysos-volume-cmd 'pamixer)
  (global-set-key (kbd "<s-next>") 'mpd-next)
  (global-set-key (kbd "<s-prior>") 'mpd-prev)
  (global-set-key (kbd "s-p") 'mpd-pause)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 Misc                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq x86-lookup-pdf "~/Documents/code/asm/Intelx86/325383-sdm-vol-2abcd.pdf"
        asm-comment-char ?\#
        twittering-use-master-password t
        edit-server-default-major-mode 'org-mode
        epa-pinentry-mode 'loopback
        paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] "
        python-shell-completion-native-disabled-interpreters '("python")
        wttrin-default-cities '("Aubervilliers" "Paris"
                                "Lyon"          "Nonières"
                                "Saint Agrève")
        prettify-symbols-alist '(("lambda" . 955) ; λ
                                 ("->" . 8594)    ; →
                                 ("<->" . 8596)   ; ↔
                                 ("<-" . 8592)    ; ←
                                 ("=>" . 8658)    ; ⇒
                                 ("<=>" . 8860)   ; ⇔
                                 ("<=" . 8656)    ; ⇐
                                 ("mapc" . 8614)  ; ↦
                                 ("map" . 8614)   ; ↦
                                 (">>" . 187)     ; »
                                 ("<<" . 171)     ; «
                                 ))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'prog-mode-hook 'eldoc-mode)
  (mapc (lambda (x)
          (add-hook 'prog-mode-hook x))
        '(visual-line-mode))
  (mapc (lambda (x)
          (add-hook x 'auto-fill-mode)
          (add-hook x 'visual-line-mode))
        '(message-mode-hook
          org-mode-hook
          text-mode-hook
          markdown-mode-hook))
  (global-aggressive-indent-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Nov-mode              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Charis SIL"
                             :size 16
                             :height 1.0))
  (mapc (lambda (mode)
          (add-hook 'nov-mode-hook mode))
        '('my-nov-font-setup
          'visual-line-mode))
  (setq nov-text-width 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            file extension           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (dolist (e '(("xml" . web-mode)    ("xinp"  . web-mode)  ("aiml" . web-mode)
               ("C"   . c++-mode)    ("dconf" . conf-mode) ("yy" . bison-mode)
               ("ll"  . flex-mode)   ("s"     . asm-mode)  ("pl" . prolog-mode)
               ("l"   . scheme-mode) ("vs"    . glsl-mode) ("fs" . glsl-mode)))
    (push (cons (concat "\\." (car e) "\\'") (cdr e)) auto-mode-alist))
  (dolist (e '("service" "timer" "target" "mount" "automount" "slice" "socket"
               "path" "netdev" "network" "link"))
    (push (cons (concat "\\." e "\\'") 'conf-unix-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              shortcuts              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "S-C-<down>") 'shrink-window)
  (global-set-key (kbd "S-C-<up>") 'enlarge-window)
  (global-set-key (kbd "C-x <up>") 'windmove-up)
  (global-set-key (kbd "C-x <down>") 'windmove-down)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x <left>") 'windmove-left)
  (global-set-key (kbd "C-<prior>") 'previous-buffer)
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key (kbd "M-»") 'end-of-buffer)
  (global-set-key (kbd "M-«") 'beginning-of-buffer)
  (global-set-key (kbd "<XF86HomePage>") 'spacemacs/home)
  (global-set-key (kbd "<XF86Open>") 'helm-find-files)
  (global-set-key (kbd "<XF86Close>") 'kill-this-buffer)
  (global-set-key (kbd "<XF86Save>") 'save-buffer)
  (global-set-key (kbd "<C-tab>") 'evil-close-fold)
  (global-set-key (kbd "<S-C-tab>") 'evil-close-folds)
  (global-set-key (kbd "<C-iso-lefttab>") 'evil-open-fold)
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/declare-prefix "oa" "applications")
  (spacemacs/declare-prefix "oc" "comments")
  (spacemacs/declare-prefix "of" "files")
  (spacemacs/declare-prefix "ofi" "i3 config")
  (spacemacs/declare-prefix "ofp" "polybar config")
  (spacemacs/declare-prefix "og" "gnus")
  (spacemacs/declare-prefix "oi" "insert")
  (spacemacs/declare-prefix "oii" "invisible space")
  (spacemacs/declare-prefix "om" "multiple-cursors")
  (spacemacs/declare-prefix "oo" "org-mode")
  (spacemacs/declare-prefix "ooi" "custom IDs")
  (spacemacs/declare-prefix "oop" "private.org")
  (spacemacs/declare-prefix "oos" "school.org")
  (spacemacs/declare-prefix "oot" "tables")
  (spacemacs/declare-prefix "oott" "toggle width")
  (spacemacs/declare-prefix "oote" "expand")
  (spacemacs/declare-prefix "oots" "shrink")
  (spacemacs/declare-prefix "or" "external command")
  (spacemacs/declare-prefix "ot" "toggle")
  (spacemacs/declare-prefix "ow" "writeroom")
  (spacemacs/set-leader-keys
    "oac" 'calc
    "oaC" 'calendar
    "oae" 'eww
    "oaf" 'fireplace
    "oaw" 'wttrin
    "ob" 'fancy-battery-mode
    "occ" 'outorg-copy-edits-and-exit
    "oce" 'outorg-edit-as-org
    "oco" 'outline-minor-mode
    "od" 'elcord-mode
    "oF" 'flycheck-mode
    "ofi" (lambda () (interactive) (find-file "~/.config/i3/config##yadm.j2"))
    "ofp" (lambda () (interactive) (find-file "~/.config/polybar/config##yadm.j2"))
    "ofo" 'find-file-at-point
    "ogd" 'turn-on-gnus-dired-mode
    "oii" (lambda () (interactive) (insert "​"))
    "ome" 'mc/edit-lines
    "omn" 'mc/mark-next-like-this
    "omp" 'mc/mark-previous-like-this
    "oma" 'mc/mark-all-like-this
    "ooi" 'eos/org-add-ids-to-headlines-in-file
    "oop" (lambda () (interactive) (find-file "~/org/private.org"))
    "oos" (lambda () (interactive) (find-file "~/org/school.org"))
    "oott" 'org-table-toggle-column-width
    "oote" 'org-table-expand
    "oots" 'org-table-shrink
    "oow" 'org-pomodoro
    "owi" 'writeroom-increase-width
    "or" 'helm-run-external-command
    "os" 'prettify-symbols-mode
    "oti" 'toggle-input-method
    "otI" 'set-input-method
    "owd" 'writeroom-decrease-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 gnus                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; get email, store in nnml
  (setq gnus-secondary-select-methods '((nnimap "1and1"
                                                (nnimap-address
                                                 "imap.1and1.fr")
                                                (nnimap-server-port 993)
                                                (nnimap-stream ssl)))
        ;; send email via 1and1
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "auth.smtp.1and1.fr"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        ;; archive outgoing emails in Sent folder on imap.1and1.fr
        gnus-message-archive-method '(nnimap "imap.1and1.fr")
        gnus-message-archive-group "Objets envoyés"
        ;; store email in ~/Mails directory
        nnml-directory "~/Mails"
        message-directory "~/Mails"
        gnus-fetch-old-headers 'some
        mm-discouraged-alternatives '("text/html" "text/richtext")
        mm-text-html-renderer 'w3m
        gnus-use-cache t)

  (gnus-add-configuration
   '(article (horizontal 1.0 (summary .4 point) (article 1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Dired                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq dired-recursive-copies (quote always)
        dired-dwim-target t
        dired-listing-switches "-ahl --group-directories-first")
  (require 'org-download)
  (add-hook 'dired-mode-hook 'org-download-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               org-mode              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (with-eval-after-load 'org

    ;; custom org functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun ck/org-confirm-babel-evaluate (lang body)
      (not (or (string= lang "latex") (string= lang "maxima"))))

    ;; custom IDs when exporting
    ;; inspired by
    ;; https://writequit.org/articles/emacs-org-mode-generate-ids.html
		(defun eos/org-id-new (&optional prefix)
			"Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a   unique   part   that   will   be   created   according   to
  `org-id-method'.

PREFIX  can specify  the  prefix,  the default  is  given by  the
variable  `org-id-prefix'.  However,  if  PREFIX  is  the  symbol
`none', don't  use any  prefix even if  `org-id-prefix' specifies
one.

So a typical ID could look like \"Org-4nd91V40HI\"."
			(let* ((prefix (if (eq prefix 'none)
												 ""
											 (concat (or prefix org-id-prefix) "-")))
						 unique)
				(if (equal prefix "-") (setq prefix ""))
				(cond
				 ((memq org-id-method '(uuidgen uuid))
					(setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
					(unless (org-uuidgen-p unique)
						(setq unique (org-id-uuid))))
				 ((eq org-id-method 'org)
					(let* ((etime (org-reverse-string (org-id-time-to-b36)))
								 (postfix (if org-id-include-domain
															(progn
																(require 'message)
																(concat "@" (message-make-fqdn))))))
						(setq unique (concat etime postfix))))
				 (t (error "Invalid `org-id-method'")))
				(concat prefix unique)))
		(defun eos/org-custom-id-get (&optional pom create prefix)
			"Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the  entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE  is non  nil, create  a  CUSTOM_ID if  none is  present
   already. PREFIX will be passed through to `eos/org-id-new'. In
   any case, the CUSTOM_ID of the entry is returned."
			(interactive)
			(org-with-point-at pom
				(let ((id (org-entry-get nil "CUSTOM_ID")))
					(cond
					 ((and id (stringp id) (string-match "\\S-" id))
						id)
					 (create
						(setq id (eos/org-id-new (concat prefix "h")))
						(org-entry-put pom "CUSTOM_ID" id)
						(org-id-add-location id (buffer-file-name (buffer-base-buffer)))
						id)))))
		(defun eos/org-add-ids-to-headlines-in-file ()
			"Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
			(interactive)
			(save-excursion
				(widen)
				(goto-char (point-min))
				(when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
					(org-map-entries (lambda () (eos/org-custom-id-get (point) 'create))))))

    ;; org-babel languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C          . t)
       (ditaa      . t)
       (dot        . t)
       (emacs-lisp . t)
       (gnuplot    . t)
       (latex      . t)
       (makefile   . t)
       (python     . t)
       (R          . t)
       (scheme     . t)
       (shell      . t)))

    ;; org hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(add-hook 'org-mode-hook
							(lambda ()
								(add-hook 'before-save-hook
													(lambda ()
														(when (and (eq major-mode 'org-mode)
																			 (eq buffer-read-only nil))
															(eos/org-add-ids-to-headlines-in-file))))))

    ;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq
     org-hide-macro-markers t
     org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
     geiser-default-implementation 'racket
     org-agenda-custom-commands '(("h" "Daily habits"
                                   ((agenda ""))
                                   ((org-agenda-show-log t)
                                    (org-agenda-ndays 7)
                                    (org-agenda-log-mode-items '(state))
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notregexp
                                                                ":DAILY:")))))
     org-agenda-files (list "~/org")
     org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate
     org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
     org-export-latex-hyperref-format "\\ref{%s}"
     org-html-validation-link nil
     org-journal-date-prefix "#+TITLE: "
     org-journal-dir "~/org/journal/"
     org-journal-file-format "%Y-%m-%d"
     org-latex-listings 'minted
     org-reveal-root "file:///home/phundrak/fromGIT/reveal.js"
     ;; org-latex-listings t
     org-latex-packages-alist '(("" "minted" t)
                                ("" "graphicx" t))
     ;; org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
     org-latex-pdf-process
     '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
     org-src-tab-acts-natively t
     user-full-name "Lucien Cartier-Tilet"
     user-mail-address "phundrak@phundrak.fr"
     ;; subscripts and superscripts need {} to work
     org-use-sub-superscripts (quote {})
     org-latex-default-packages-alist '((""         "graphicx"  t)
                                        (""         "grffile"   t)
                                        (""         "longtable" nil)
                                        (""         "wrapfig"   nil)
                                        (""         "rotating"  nil)
                                        ("normalem" "ulem"      t)
                                        (""         "amsmath"   t)
                                        (""         "textcomp"  t)
                                        (""         "amssymb"   t)
                                        (""         "capt-of"   nil)
                                        (""         "hyperref"  nil))
     org-structure-template-alist (append
                                   org-structure-template-alist
                                   '(("el" "#+BEGIN_SRC emacs-lisp :exports results
?
#+END_SRC")))
     ;;; Org projects
     org-publish-project-alist
     '(("langue-phundrak-fr-html"
        :base-directory "~/Documents/conlanging/"
        :base-extension "org"
        :exclude "\\./\\(CONTRIB\\|README\\|head\\|temp\\|svg-ink\\).*"
        :publishing-directory "/ssh:Naro:~/www/phundrak.fr/langue"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 5
        :auto-preamble t)
       ("langue-phundrak-fr-pdf"
        :base-directory "~/Documents/conlanging/"
        :base-extension "org"
        :exclude "\\./\\(CONTRIB\\|README\\|index\\|head\\|temp\\|svg-ink\\).*"
        :publishing-directory "/ssh:Naro:~/www/phundrak.fr/langue"
        :recursive t
        :publishing-function org-latex-publish-to-pdf
        :headline-levels 5
        :auto-preamble t)
       ("langue-phundrak-fr-static"
        :base-directory "~/Documents/conlanging"
        :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|jpeg\\|ttf\\|woff\\|txt"
        :exclude ".*auto-generated.*"
        :publishing-directory "/ssh:Naro:~/www/phundrak.fr/langue"
        :recursive t
        :publishing-function org-publish-attachment)
       ("langue-phundrak-fr"
        :components ("langue-phundrak-fr-html"
                     "langue-phundrak-fr-static"
                     "langue-phundrak-fr-pdf"))))

    ;; Shortcuts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (with-eval-after-load 'org-agenda
      (require 'org-projectile)
      (mapcar #'(lambda (file)
                  (when (file-exists-p file)
                    (push file org-agenda-files)))
              (org-projectile-todo-files)))
    (eval-after-load "ox-latex"
      ;; update the list of LaTeX classes and associated header (encoding, etc.)
      ;; and structure
      '(add-to-list 'org-latex-classes
                    '("conlang"
                      "\\documentclass{book}"
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                    `("beamer"
                      ,(concat "\\documentclass[presentation]{beamer}\n"
                               "[DEFAULT-PACKAGES]"
                               "[PACKAGES]"
                               "[EXTRA]\n")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                LaTeX                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defcustom tex-my-viewer
    "xreader --fork -s -x \"emacsclient --eval '(progn (switch-to-buffer (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\""
    "PDF  Viewer for  TeX documents.  You  may want  to fork  the
viewer  so that  it detects  when the  same document  is launched
twice, and persist when Emacs gets closed.

Simple command:

  xreader --fork

We can use

  emacsclient --eval '(progn (switch-to-buffer
                       (file-name-nondirectory \"%{input}\"))
                      (goto-line %{line}))'

to reverse-search a  pdf using SyncTeX. Note that  the quotes and
double-quotes matter and must be escaped appropriately."
    :safe 'stringp)
  (add-hook 'doc-view-minor-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                  Qt                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq
   c-C++-access-key
   "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
  (font-lock-add-keywords
   'c++-mode
   '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               flycheck              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; force flycheck to always use c++11 support. We use the
  ;; clang language backend so this is set to clang
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++17")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 rust                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path (concat
                             (getenv "HOME")
                             "/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
  (add-hook 'rust-mode-hook
            '(lambda ()
               (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
               (electric-pair-mode 1)
               (indent-guide-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                scheme               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq geiser-chicken-binary "chicken-csi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 Java                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq eclimd-default-workspace "/home/phundrak/eclipse-workspace")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Eshell               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defadvice find-file (around find-files activate)
    "Also find all files within a list of files. This even works recursively."
    (if (listp filename)
        (loop for f in filename do (find-file f wildcards))
      ad-do-it))
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))
  (defalias 'open 'find-file)
  (defalias 'openo 'find-file-other-window)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'list-buffers 'ibuffer)
  (defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))
  (defun eshell/abbr-pwd ()
    (let ((home (getenv "HOME"))
          (path (eshell/pwd)))
      (cond
       ((string-equal home path) "~")
       ((f-ancestor-of? home path) (concat "~/" (f-relative path home)))
       (path))))
  (defun eshell/my-prompt ()
    (let ((header-bg "#161616"))
      (concat
       (with-face (eshell/abbr-pwd) :foreground "#008700")
       "\n"
       (if (= (user-uid) 0)
           (with-face "➜" :foreground "red")
         (with-face "➜" :foreground "#2345ba"))
       " ")))
  (setq eshell-visual-commands
        '("fish" "zsh" "bash" "tmux" "htop" "top" "vim" "bat" "nano")
        eshell-visual-subcommands
        '("git" "log" "l" "diff" "show")
        eshell-prompt-regexp "^[^#$\n]*[#$] "
        eshell-prompt-function 'eshell/my-prompt)
  (eshell-git-prompt-use-theme 'powerline))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(evil-want-Y-yank-to-eol nil)
 '(org-export-headline-levels 4)
 '(package-selected-packages
   '(xkcd vmd-mode visual-fill-column typit mmt sudoku restclient-helm pony-mode pacmacs ox-reveal outorg ob-restclient ob-http meson-mode ibuffer-projectile lv helm-w3m w3m graphviz-dot-mode flycheck-gometalinter transient ess-smart-equals ess-R-data-view ctable ess julia-mode eshell-git-prompt emoji-cheat-sheet-plus edit-indirect dockerfile-mode docker docker-tramp company-restclient restclient know-your-http-well company-quickhelp company-emoji company-emacs-eclim eclim atomic-chrome websocket 2048-game ox-gfm slime-company slime common-lisp-snippets erlang insert-shebang fish-mode company-shell faceup racket-mode treepy graphql yapfify yaml-mode xterm-color web-beautify twittering-mode toml-mode tagedit stickyfunc-enhance smeargle slim-mode shell-pop selectric-mode scss-mode sass-mode ranger rainbow-identifiers pytest pyenv-mode py-isort pug-mode plantuml-mode phpunit phpcbf php-auto-yasnippets pdf-tools tablist ox-pandoc orgit org-present org-pomodoro alert log4e gntp ob-elixir multi-term markdown-toc magit-gitflow magit-gh-pulls livid-mode live-py-mode json-snatcher js2-refactor js-doc htmlize hlint-refactor hindent helm-pydoc helm-hoogle helm-gitignore helm-css-scss haskell-snippets haml-mode gnuplot glsl-mode gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-messenger gist gh marshal logito pcache ht gh-md flyspell-correct-helm flyspell-correct flycheck-rust pos-tip flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help drupal-mode disaster cython-mode dash-functional tern company-ghci company-ghc ghc color-identifiers-mode cmm-mode clang-format cargo auto-dictionary alchemist modern-cpp-font-lock yasnippet-snippets x86-lookup web-mode srefactor racer pyvenv pip-requirements pandoc-mode org-projectile org-category-capture org-mime org-download nasm-mode json-reformat intero imenu-list hy-mode git-timemachine git-link geiser flycheck-pos-tip flycheck-haskell evil-magit emmet-mode cmake-mode anaconda-mode rust-mode elixir-mode flycheck haskell-mode multiple-cursors skewer-mode simple-httpd markdown-mode magit magit-popup git-commit ghub with-editor pythonic emms gmail-message-mode ham-mode html-to-markdown flymd edit-server image-dired+ go-guru go-eldoc company-go go-mode unfill mwim company-web web-completion-data company-tern company-cabal company-c-headers company-auctex company-anaconda elcord xresources-theme sql-indent rainbow-mode php-extras php-mode mmm-mode json-mode js2-mode csv-mode coffee-mode auctex helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(default-input-method "ipa-x-sampa")
 '(eshell-aliases-file "/home/phundrak/.emacs.d/private/eshell-alias")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#5B6268")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-export-headline-levels 4)
 '(package-selected-packages
   (quote
    (jinja2-mode company-ansible ansible-doc ansible xkcd vmd-mode visual-fill-column typit mmt sudoku restclient-helm pony-mode pacmacs ox-reveal outorg ob-restclient ob-http meson-mode ibuffer-projectile lv helm-w3m w3m graphviz-dot-mode flycheck-gometalinter transient ess-smart-equals ess-R-data-view ctable ess julia-mode eshell-git-prompt emoji-cheat-sheet-plus edit-indirect dockerfile-mode docker docker-tramp company-restclient restclient know-your-http-well company-quickhelp company-emoji company-emacs-eclim eclim atomic-chrome websocket 2048-game ox-gfm slime-company slime common-lisp-snippets erlang insert-shebang fish-mode company-shell faceup racket-mode treepy graphql yapfify yaml-mode xterm-color web-beautify twittering-mode toml-mode tagedit stickyfunc-enhance smeargle slim-mode shell-pop selectric-mode scss-mode sass-mode ranger rainbow-identifiers pytest pyenv-mode py-isort pug-mode plantuml-mode phpunit phpcbf php-auto-yasnippets pdf-tools tablist ox-pandoc orgit org-present org-pomodoro alert log4e gntp ob-elixir multi-term markdown-toc magit-gitflow magit-gh-pulls livid-mode live-py-mode json-snatcher js2-refactor js-doc htmlize hlint-refactor hindent helm-pydoc helm-hoogle helm-gitignore helm-css-scss haskell-snippets haml-mode gnuplot glsl-mode gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-messenger gist gh marshal logito pcache ht gh-md flyspell-correct-helm flyspell-correct flycheck-rust pos-tip flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help drupal-mode disaster cython-mode dash-functional tern company-ghci company-ghc ghc color-identifiers-mode cmm-mode clang-format cargo auto-dictionary alchemist modern-cpp-font-lock yasnippet-snippets x86-lookup web-mode srefactor racer pyvenv pip-requirements pandoc-mode org-projectile org-category-capture org-mime org-download nasm-mode json-reformat intero imenu-list hy-mode git-timemachine git-link geiser flycheck-pos-tip flycheck-haskell evil-magit emmet-mode cmake-mode anaconda-mode rust-mode elixir-mode flycheck haskell-mode multiple-cursors skewer-mode simple-httpd markdown-mode magit magit-popup git-commit ghub with-editor pythonic emms gmail-message-mode ham-mode html-to-markdown flymd edit-server image-dired+ go-guru go-eldoc company-go go-mode unfill mwim company-web web-completion-data company-tern company-cabal company-c-headers company-auctex company-anaconda elcord xresources-theme sql-indent rainbow-mode php-extras php-mode mmm-mode json-mode js2-mode csv-mode coffee-mode auctex helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
     (javascript-backend . tern)
     (javascript-backend . lsp)
     (go-backend . go-mode)
     (go-backend . lsp))))
 '(solaire-mode-auto-swap-bg t)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
