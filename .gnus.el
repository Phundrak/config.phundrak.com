(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

;; get email, store in nnml
(setq gnus-secondary-select-methods '((nnimap "lucien@phundrak.com"
                                              (nnimap-address "mail.phundrak.com")
                                              (nnimap-server-port 143)
                                              (nnimap-stream starttls)))
      ;; send email via 1and1
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.phundrak.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      ;; archive outgoing emails in Sent folder on imap.1and1.fr
      gnus-message-archive-method '(nnimap "mail.phundrak.com")
      gnus-message-archive-group "Sent"
      ;; store email in ~/Mails directory
      nnml-directory "~/Mails"
      message-directory "~/Mails"
      gnus-fetch-old-headers 'some
      mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-text-html-renderer 'w3m
      gnus-use-cache t)

(gnus-add-configuration
 '(article (horizontal 1.0 (summary .4 point) (article 1.0))))

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("lucien@phundrak.com" visible nil nil))))
     (setq gnus-topic-alist '(("lucien@phundrak.com" ; the key of the topic
                               "nnimap+lucien@phundrak.com:INBOX"
                               "nnimap+lucien@phundrak.com:Sent"
                               "nnimap+lucien@phundrak.com:Drafts")
                              ("Gnus")))))
