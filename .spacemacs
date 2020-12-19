;; -*- mode: emacs-lisp; lexical-binding: t -*-
(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (setq phundrak//dotspacemacs-src-dir "~/.config/emacs/private/"
        phundrak//dotspacemacs-src "~/org/config/emacs.org"
        phundrak//dotspacemacs-si (concat phundrak//dotspacemacs-src-dir "spacemacs-init.el")
        phundrak//dotspacemacs-sl (concat phundrak//dotspacemacs-src-dir "spacemacs-layers.el")
        phundrak//dotspacemacs-uc (concat phundrak//dotspacemacs-src-dir "user-config.el")
        phundrak//dotspacemacs-ui (concat phundrak//dotspacemacs-src-dir "user-init.el"))
  (when (or (file-newer-than-file-p phundrak//dotspacemacs-src phundrak//dotspacemacs-si)
            (file-newer-than-file-p phundrak//dotspacemacs-src phundrak//dotspacemacs-sl)
            (file-newer-than-file-p phundrak//dotspacemacs-src phundrak//dotspacemacs-ui)
            (file-newer-than-file-p phundrak//dotspacemacs-src phundrak//dotspacemacs-uc))
    (princ "Exporting new Emacs configuration from spacemacs.org through org-babel...")
    (call-process
     (concat invocation-directory invocation-name)
     nil nil t
     "-q" "--batch" "--eval" "(require 'ob-tangle)"
     "--eval" (format "(org-babel-tangle-file \"%s\")" phundrak//dotspacemacs-src))
    (princ "done"))
  (load-file phundrak//dotspacemacs-si))

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (load-file phundrak//dotspacemacs-sl))

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
  (load-file phundrak//dotspacemacs-ui))

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
  (load-file phundrak//dotspacemacs-uc))


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
   '(("TODO" . "#dc752f")
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
     ("XXXX" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-export-headline-levels 4)
 '(package-selected-packages
   '(caddyfile-mode ob-swift wttrin wrap-region pinentry org-tree-slide org-sidebar org-ql peg ov org-super-agenda ts dired-du caddyfile-mode loop xkcd vmd-mode visual-fill-column typit mmt sudoku restclient-helm pony-mode pacmacs ox-reveal outorg ob-restclient ob-http meson-mode ibuffer-projectile lv helm-w3m w3m graphviz-dot-mode flycheck-gometalinter transient ess-smart-equals ess-R-data-view ctable ess julia-mode eshell-git-prompt emoji-cheat-sheet-plus edit-indirect dockerfile-mode docker docker-tramp company-restclient restclient know-your-http-well company-quickhelp company-emoji company-emacs-eclim eclim atomic-chrome websocket 2048-game ox-gfm slime-company slime common-lisp-snippets erlang insert-shebang fish-mode company-shell faceup racket-mode treepy graphql yapfify yaml-mode xterm-color web-beautify twittering-mode toml-mode tagedit stickyfunc-enhance smeargle slim-mode shell-pop selectric-mode scss-mode sass-mode ranger rainbow-identifiers pytest pyenv-mode py-isort pug-mode plantuml-mode phpunit phpcbf php-auto-yasnippets pdf-tools tablist ox-pandoc orgit org-present org-pomodoro alert log4e gntp ob-elixir multi-term markdown-toc magit-gitflow magit-gh-pulls livid-mode live-py-mode json-snatcher js2-refactor js-doc htmlize hlint-refactor hindent helm-pydoc helm-hoogle helm-gitignore helm-css-scss haskell-snippets haml-mode gnuplot glsl-mode gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-messenger gist gh marshal logito pcache ht gh-md flyspell-correct-helm flyspell-correct flycheck-rust pos-tip flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help drupal-mode disaster cython-mode dash-functional tern company-ghci company-ghc ghc color-identifiers-mode cmm-mode clang-format cargo auto-dictionary alchemist modern-cpp-font-lock yasnippet-snippets x86-lookup web-mode srefactor racer pyvenv pip-requirements pandoc-mode org-projectile org-category-capture org-mime org-download nasm-mode json-reformat intero imenu-list hy-mode git-timemachine git-link geiser flycheck-pos-tip flycheck-haskell evil-magit emmet-mode cmake-mode anaconda-mode rust-mode elixir-mode flycheck haskell-mode multiple-cursors skewer-mode simple-httpd markdown-mode magit magit-popup git-commit ghub with-editor pythonic emms gmail-message-mode ham-mode html-to-markdown flymd edit-server image-dired+ go-guru go-eldoc company-go go-mode unfill mwim company-web web-completion-data company-tern company-cabal company-c-headers company-auctex company-anaconda elcord xresources-theme sql-indent rainbow-mode php-extras php-mode mmm-mode json-mode js2-mode csv-mode coffee-mode auctex helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(safe-local-variable-values
   '((org-confirm-babel-evaluate)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
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
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch :height 0.8))))
 '(org-block-begin-line ((t (:inherit fixed-pitch :height 0.8))))
 '(org-checkbox ((t (:inherit (org-todo shadow fixed-pitch)))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "#d08770"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#bf616a" :height 2.0 :italic t))))
 '(org-drawer ((t (:inherit fixed-pitch :height 0.8 :foreground "#5e81ac"))))
 '(org-formula ((t (:inherit fixed-pitch :height 0.8))))
 '(org-indent ((t (:inherit fixed-pitch :height 0.8))))
 '(org-level-1 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#b48ead" :height 1.75))))
 '(org-level-2 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#5e81ac" :height 1.5))))
 '(org-level-3 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#81a1c1" :height 1.25))))
 '(org-level-4 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#b48ead" :height 1.1))))
 '(org-level-5 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#88c0d0"))))
 '(org-level-6 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#8fbcbb"))))
 '(org-level-7 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#b48ead"))))
 '(org-level-8 ((t (:font "Charis SIL" :height 1.0 :inherit default :weight bold :foreground "#eceff4"))))
 '(org-link ((t (:foreground "#88c0d0" :underline t))))
 '(org-macro ((t (:inherit fixed-pitch :height 0.8))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
 '(org-property-value ((t (:inherit fixed-pitch :height 0.8))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8 :foreground "#b48ead"))))
 '(org-table ((t (:inherit fixed-pitch :height 0.8 :foreground "#a3be8c"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-target ((t (:inherit fixed-pitch :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:font "Charis SIL" :height 1.0 :height 1.3 :weight thin)))))
)
