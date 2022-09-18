(setq user-mail-address "kristoffer@kortbaek.dk"
      user-full-name    "Kristoffer August Korbaek")

(setq gnus-select-method
      '(nnimap "imap.fastmail.com"
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnimap-split-methods default)))

(setq smtpmail-smtp-service 465)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.fastmail.com")

