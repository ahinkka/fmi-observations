(asdf:defsystem #:fmi-observations
  :depends-on (#:babel
	       #:cxml-stp
	       #:xpath
	       #:parse-number
	       #:local-time
	       #:drakma
	       #:iterate)
  :components ((:file "fmi-observations")))