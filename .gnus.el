(setq user-mail-address "kristoffer@kortbaek.dk"
      user-full-name    "Kristoffer August Korbaek")

(setq gnus-select-method
      '(nnimap "imap.fastmail.com"
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnimap-split-methods
		'(("mail.news", "^From:.*slashdot.*")
		  ("mail.news", "^From:.*Morning Brew.*")
		  ("mail.other", "")))))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "KU-mail"
		      (nnimap-inbox "INBOX")
		      (nnimap-address "exchange.ku.dk")
		      (nnimap-server-port 993)
		      (nnimap-authenticator login)
		      (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods
	     '(nntp "news.eternal-september.org"
		    (nntp-open-connection-function nntp-open-tls-stream)
		    (nntp-port-number 563)
		    (nntp-address "news.eternal-september.org")))
(add-to-list 'gnus-secondary-select-methods
	     '(nntp "news.gmane.io"))
 
(setq smtpmail-smtp-service 465)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.fastmail.com")

(setq gnus-summary-line-format "%o%U%R%z%B%(%[%4L: %-23,23f%]%) %s\n"
      gnus-large-newsgroup 200
      large-newsgroup-initial 200)
