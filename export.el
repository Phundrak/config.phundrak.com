#!/usr/bin/env -S emacs -Q --script
(require 'package)
(require 'org)
(require 'ox-html)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents)
(package-install 'htmlize)
(setq org-confirm-babel-evaluate nil
      org-html-validation-link nil)
(let ((files (mapcar #'expand-file-name
                     (file-expand-wildcards "org/config/*.org"))))
  (mapc (lambda (file)
          (message (format "==========\nExporting %S\n==========" file))
          (with-current-buffer (find-file file)
            (org-html-export-to-html)))
        files))
(let* ((files (mapcar #'expand-file-name
                      (file-expand-wildcards "org/config/*.html~"))))
  (mapc (lambda (file)
          (delete-file file))
        files))
