(asdf:defsystem #:fmi-observations
  :depends-on (#:babel
	       #:cxml-stp
	       #:xpath
	       #:parse-number
	       #:local-time
	       #:drakma
	       #:iterate
	       #:fare-mop)
  :components ((:file "package")
	       (:file "point")
	       (:file "criteria")
	       (:file "fmi-observations")))
