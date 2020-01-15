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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(asm
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      :disabled-for
                      org
                      git)
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c-mode
            c-c++-adopt-subprojects t
            c-c++-enable-google-style t
            c-c++-enable-c++11 t
            c-c++-backend 'lsp-ccls
            c-c++-lsp-executable "/usr/bin/ccls"
            c-c++-lsp-sem-highlight-method 'overlay
            c-c++-lsp-sem-highlight-rainbow t
            c-c++-adopt-subprojects t
            c++-enable-organize-includes-on-save t)
     (cmake :variables
            cmake-enable-cmake-ide-support t)
     conlanging
     csv
     colors
     (dart :variables
           dart-server-sdk-path "/opt/flutter/bin/cache/dart-sdk/"
           lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk/")
     dap
     dired-phundrak
     django
     docker
     emacs-lisp
     epub
     ess
     git
		 graphviz
     (go :variables
         go-backend 'lsp
				 go-tab-width 2
         go-use-golangci-lint t)
     gnus
     (helm :variables
           helm-no-header t
           helm-use-fuzzy 'source)
     helpful
     (html :variables
           web-fmt-tool 'web-beautify
           css-enable-lsp t
           less-enable-lsp t
           scss-enable-lsp t
           html-enable-lsp t)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     imenu-list
     (javascript :variables
                 javascript-backend 'lsp
                 javascript-lsp-linter nil
                 javascript-fmt-tool 'web-beautify
                 javascript-repl 'skewer
                 node-add-modules-path t)
     (json :variables
           json-fmt-tool 'web-beautify)
     (keyboard-layout :variables
                      kl-layout 'bepo
                      kl-disabled-configurations '(magit dired eww))
     (latex :variables
            latex-build-command "xelatex"
            latex-enable-auto-fill t
            latex-enable-folding t
            latex-enable-magic t)
     lsp
     major-modes
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
          org-download-image-dir "~/Pictures/org/"
          org-enable-org-journal-support t
          org-journal-dir "~/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-enable-epub-support t
          org-return-follows-link t)
     pass
     pdf
     prettier
     prolog
     (python :variables
             python-backend 'lsp
             python-sort-imports-on-save t
             python-fill-column 80
             python-test-runner '(pytest nose)
             python-formatter 'lsp)
     (restclient :variables
                 restclient-use-org t)
     (rust :variables rust-backend 'lsp)
     (scheme :variables
             geiser-chicken-binary "chicken-csi")
     semantic
     (shell :variables
            shell-default-height 40
            shell-default-position 'bottom
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
     xkcd
     web-beautify
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(atomic-chrome
                                      dired-du
                                      doom-themes
                                      edit-indirect
                                      elcord
                                      eshell-git-prompt
                                      flycheck-golangci-lint
                                      kaolin-themes
                                      magit-gitflow
                                      meson-mode
                                      modern-cpp-font-lock
                                      multiple-cursors
                                      org-sidebar
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
  (setq phundrak/src-dir (concat (getenv "HOME") "/.emacs.d/private/")
        phundrak/gnus-uc (concat (getenv "HOME") "/.gnus.el")
        phundrak/src (concat (getenv "HOME") "/spacemacs.org")
        phundrak/si (concat phundrak/src-dir "spacemacs-init.el")
        phundrak/uc (concat phundrak/src-dir "user-config.el")
        phundrak/ui (concat phundrak/src-dir "user-init.el"))
  (when (or (file-newer-than-file-p phundrak/src phundrak/si)
            (file-newer-than-file-p phundrak/src phundrak/ui)
            (file-newer-than-file-p phundrak/src phundrak/uc)
            (file-newer-than-file-p phundrak/src phundrak/gnus-uc))
    (message "Exporting new Emacs configuration from spacemacs.org through Org-babel")
    (call-process
     (concat invocation-directory invocation-name)
     nil nil t
     "-q" "--batch" "--eval" "(require 'ob-tangle)"
     "--eval" (format "(org-babel-tangle-file \"%s\")" phundrak/src)))

  (load-file phundrak/si))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (load-file phundrak/ui))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; load file exported from `~/spacemacs.org' in `dotspacemacs/user-load'
  (load-file phundrak/uc))


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
      (helpful elisp-refs loop xkcd vmd-mode visual-fill-column typit mmt sudoku restclient-helm pony-mode pacmacs ox-reveal outorg ob-restclient ob-http meson-mode ibuffer-projectile lv helm-w3m w3m graphviz-dot-mode flycheck-gometalinter transient ess-smart-equals ess-R-data-view ctable ess julia-mode eshell-git-prompt emoji-cheat-sheet-plus edit-indirect dockerfile-mode docker docker-tramp company-restclient restclient know-your-http-well company-quickhelp company-emoji company-emacs-eclim eclim atomic-chrome websocket 2048-game ox-gfm slime-company slime common-lisp-snippets erlang insert-shebang fish-mode company-shell faceup racket-mode treepy graphql yapfify yaml-mode xterm-color web-beautify twittering-mode toml-mode tagedit stickyfunc-enhance smeargle slim-mode shell-pop selectric-mode scss-mode sass-mode ranger rainbow-identifiers pytest pyenv-mode py-isort pug-mode plantuml-mode phpunit phpcbf php-auto-yasnippets pdf-tools tablist ox-pandoc orgit org-present org-pomodoro alert log4e gntp ob-elixir multi-term markdown-toc magit-gitflow magit-gh-pulls livid-mode live-py-mode json-snatcher js2-refactor js-doc htmlize hlint-refactor hindent helm-pydoc helm-hoogle helm-gitignore helm-css-scss haskell-snippets haml-mode gnuplot glsl-mode gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-messenger gist gh marshal logito pcache ht gh-md flyspell-correct-helm flyspell-correct flycheck-rust pos-tip flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help drupal-mode disaster cython-mode dash-functional tern company-ghci company-ghc ghc color-identifiers-mode cmm-mode clang-format cargo auto-dictionary alchemist modern-cpp-font-lock yasnippet-snippets x86-lookup web-mode srefactor racer pyvenv pip-requirements pandoc-mode org-projectile org-category-capture org-mime org-download nasm-mode json-reformat intero imenu-list hy-mode git-timemachine git-link geiser flycheck-pos-tip flycheck-haskell evil-magit emmet-mode cmake-mode anaconda-mode rust-mode elixir-mode flycheck haskell-mode multiple-cursors skewer-mode simple-httpd markdown-mode magit magit-popup git-commit ghub with-editor pythonic emms gmail-message-mode ham-mode html-to-markdown flymd edit-server image-dired+ go-guru go-eldoc company-go go-mode unfill mwim company-web web-completion-data company-tern company-cabal company-c-headers company-auctex company-anaconda elcord xresources-theme sql-indent rainbow-mode php-extras php-mode mmm-mode json-mode js2-mode csv-mode coffee-mode auctex helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
   '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
   '(safe-local-variable-values
     (quote
      ((org-confirm-babel-evaluate)
       (javascript-backend . tern)
       (javascript-backend . lsp)
       (go-backend . go-mode)
       (go-backend . lsp))))
   '(send-mail-function (quote smtpmail-send-it))
   '(smtpmail-smtp-server "mail.phundrak.com")
   '(smtpmail-smtp-service 587)
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
