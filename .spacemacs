;; -*- mode: emacs-lisp; lexical-binding: t -*-
(defvar phundrak--dotspacemacs-src-dir "~/.config/emacs/private/"
  "Directory for my exported Elisp configuration files")
(defvar phundrak--dotspacemacs-src "~/org/config/emacs.org"
  "My litterate config file for Emacs")
(defvar phundrak--dotspacemacs-si (concat phundrak--dotspacemacs-src-dir "spacemacs-init"))
(defvar phundrak--dotspacemacs-sl (concat phundrak--dotspacemacs-src-dir "spacemacs-layers"))
(defvar phundrak--dotspacemacs-uc (concat phundrak--dotspacemacs-src-dir "user-config"))
(defvar phundrak--dotspacemacs-ui (concat phundrak--dotspacemacs-src-dir "user-init"))
(defvar phundrak--dotspacemacs-ul (concat phundrak--dotspacemacs-src-dir "user-load"))
(defvar phundrak--dotspacemacs-files (list phundrak--dotspacemacs-si phundrak--dotspacemacs-sl
                                           phundrak--dotspacemacs-uc phundrak--dotspacemacs-ui
                                           phundrak--dotspacemacs-ul))

;; turn off native comp warnings
(setq comp-async-report-warnings-errors nil)

(defun phundrak-update-config-files-p (&optional compiled?)
  "Verify if any of my exported Elisp configuration files are
newer than my litterate configuration.

If `compiled?' is `t', check the `.elc' files instead of the
`.el' files."
  (catch 'ret
    (dolist (file phundrak--dotspacemacs-files)
      (when (file-newer-than-file-p phundrak--dotspacemacs-src
                                    (format "%s.%s"
                                            file
                                            (if compiled? "elc" "el")))
        (throw 'ret t)))))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."

  (when (phundrak-update-config-files-p)
    (message "Exporting new Emacs configuration from spacemacs.org through org-babel...")
    (require 'ob-tangle)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle-file phundrak--dotspacemacs-src))
    (message "Exporting new Emacs configuration from spacemacs.org through org-babel...done"))
  (load phundrak--dotspacemacs-si))

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (load phundrak--dotspacemacs-sl))

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
  (load phundrak--dotspacemacs-ui))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  (load phundrak--dotspacemacs-ul))

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (load phundrak--dotspacemacs-uc))

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
 '(mml-secure-openpgp-sign-with-sender t)
 '(objed-cursor-color "#ff6c6b")
 '(org-export-headline-levels 4)
 '(package-selected-packages
   '(org-appear yasnippet-snippets yapfify yaml-mode xkcd x86-lookup wttrin xterm-color ws-butler wrap-region winum which-key web-mode web-beautify volatile-highlights vmd-mode visual-fill-column vi-tilde-fringe uuidgen use-package unfill undo-tree twittering-mode toml-mode toc-org tide typescript-mode tagedit systemd stickyfunc-enhance srefactor spaceline smeargle slim-mode sicp shell-pop selectric-mode scss-mode sass-mode restclient-helm restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode pony-mode plantuml-mode pip-requirements pinentry persp-mode pcre2el paradox spinner ox-ssh ox-reveal ox-gfm outorg orgit org-tree-slide org-sidebar org-ql peg ov org-super-agenda ts org-ref pdf-tools key-chord ivy org-projectile org-category-capture org-present org-pomodoro org-mime org-download org-bullets open-junk-file ob-restclient ob-latex-as-png ob-http nginx-mode neotree nasm-mode mwim multi-term mu4e-maildirs-extension mu4e-alert ht alert log4e gntp move-text mmm-mode markdown-toc magit-gitflow magit-popup magit macrostep lua-mode lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative link-hint js2-refactor multiple-cursors js2-mode js-doc insert-shebang info-colors indent-guide imenu-list ibuffer-projectile hydra lv hy-mode dash-functional hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-bibtex bibtex-completion parsebib helm-ag haml-mode graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit with-editor gh-md geiser fuzzy flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip flycheck pkg-info epl flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eval-sexp-fu eshell-z eshell-syntax-highlighting eshell-prompt-extras esh-help emmet-mode elisp-slime-nav elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet powerline popwin elfeed elcord edit-indirect dumb-jump dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat disaster diredfl dired-git-info diminish define-word cython-mode csv-mode company-web web-completion-data company-statistics company-shell company-restclient restclient know-your-http-well company-quickhelp pos-tip company-emacs-eclim eclim company-c-headers company-auctex company-anaconda company column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format cargo markdown-mode rust-mode caddyfile-mode loop bind-map bind-key biblio biblio-core auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup nord-theme))
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
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(yasnippet-snippets yapfify yaml-mode xkcd x86-lookup wttrin xterm-color ws-butler wrap-region winum which-key web-mode web-beautify volatile-highlights vmd-mode visual-fill-column vi-tilde-fringe uuidgen use-package unfill undo-tree twittering-mode toml-mode toc-org tide typescript-mode tagedit systemd stickyfunc-enhance srefactor spaceline smeargle slim-mode sicp shell-pop selectric-mode scss-mode sass-mode restclient-helm restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode pony-mode plantuml-mode pip-requirements pinentry persp-mode pcre2el paradox spinner ox-ssh ox-reveal ox-gfm outorg orgit org-tree-slide org-sidebar org-ql peg ov org-super-agenda ts org-ref pdf-tools key-chord ivy org-projectile org-category-capture org-present org-pomodoro org-mime org-download org-bullets open-junk-file ob-restclient ob-latex-as-png ob-http nginx-mode neotree nasm-mode mwim multi-term mu4e-maildirs-extension mu4e-alert ht alert log4e gntp move-text mmm-mode markdown-toc magit-gitflow magit-popup magit macrostep lua-mode lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative link-hint js2-refactor multiple-cursors js2-mode js-doc insert-shebang info-colors indent-guide imenu-list ibuffer-projectile hydra lv hy-mode dash-functional hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-bibtex bibtex-completion parsebib helm-ag haml-mode graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit with-editor gh-md geiser fuzzy flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip flycheck pkg-info epl flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eval-sexp-fu eshell-z eshell-syntax-highlighting eshell-prompt-extras esh-help emmet-mode elisp-slime-nav elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet powerline popwin elfeed elcord edit-indirect dumb-jump dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat disaster diredfl dired-git-info diminish define-word cython-mode csv-mode company-web web-completion-data company-statistics company-shell company-restclient restclient know-your-http-well company-quickhelp pos-tip company-emacs-eclim eclim company-c-headers company-auctex company-anaconda company column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format cargo markdown-mode rust-mode caddyfile-mode loop bind-map bind-key biblio biblio-core auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup nord-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
