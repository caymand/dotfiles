(setq user-mail-address "kristoffer@kortbaek.dk"
      user-full-name    "Kristoffer August Korbaek")

(setq gnus-select-method
      '(nnimap "imap.fastmail.com"
	       (nnimap-inbox "INBOX")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnimap-split-methods default)))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "KU-mail"
		      (nnimap-inbox "INBOX")
		      (nnimap-address "exchange.ku.dk")
		      (nnimap-server-port 993)
		      (nnimap-authenticator login)
		      (nnimap-stream ssl)))
 
(setq smtpmail-smtp-service 465)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.fastmail.com")

(setq gnus-summary-line-format "%d%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
