;;;; Copyright (c) 2013, Atte Hinkka <atte.hinkka@iki.fi>
;;;; 
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage #:fmi-observations
  (:use #:common-lisp)
  (:export :get-weather))

(in-package :fmi-observations)

(defun string-to-dom (string)
  (cxml:parse-octets
   (babel:string-to-octets string)
   (cxml-dom:make-dom-builder)))

(defun extract-node-values (dom xpath-expression)
  (xpath:with-namespaces (("gml" "http://www.opengis.net/gml/3.2")
			  ("target" "http://xml.fmi.fi/namespace/om/atmosphericfeatures/0.95")
			  ("gmlcov" "http://www.opengis.net/gmlcov/1.0"))
    (let ((node-set (xpath:evaluate xpath-expression dom)))
      (xpath:map-node-set->list
       #'(lambda (node) (xpath:evaluate "string()" node))
       node-set))))

(defun get-weather-data (place-name api-key time-step)
  (let* ((url
	  (format nil
		  "http://data.fmi.fi/fmi-apikey/~A/wfs?request=getFeature&storedquery_id=fmi::observations::weather::realtime::place::multipointcoverage&place=~A&timestep=~A"
		  api-key
		  (drakma:url-encode place-name :utf-8)
		  time-step))
	 (xml (drakma:http-request url :external-format-out :utf-8 :external-format-in :utf-8))
	 (dom (string-to-dom xml)))
    (values
     (list
      (extract-node-values dom
			   "//target:region[@codeSpace='http://xml.fmi.fi/namespace/location/region']")
      (extract-node-values dom
			   "//gml:name[@codeSpace='http://xml.fmi.fi/namespace/locationcode/name']")
      (extract-node-values dom
			   "//gmlcov:positions")
      (extract-node-values dom
			   "//gml:doubleOrNilReasonTupleList"))
     url)))

(defun weather-string-to-list (string)
  (remove-if
   #'(lambda (line) (= 0 (length line)))
   (mapcar #'(lambda (line)
	       (string-trim " " line))
	   (cl-ppcre:split "\\n" string))))

(defun item-from-weather-string (string position-function)
  (mapcar #'(lambda (line)
	      (funcall position-function (remove-if #'(lambda (item) (= 0 (length item)))
							 (cl-ppcre:split " " line))))
	  (weather-string-to-list string)))

(defun temperature-tuples (weather-positions weather-observations)
  (mapcar #'list
	  (item-from-weather-string weather-positions
				    #'(lambda (item) (local-time:unix-to-timestamp
						      (parse-integer (car (last item))))))
	  (item-from-weather-string weather-observations
				    #'(lambda (item) 
					(handler-case 
					    (parse-number:parse-number (first item))
					  (sb-int:simple-parse-error (s-p-e)
					    (declare (ignore s-p-e))
					    nil))))))

(defun sort-temperature-tuples (tuples)
  (sort tuples
	#'(lambda (x y) (local-time:timestamp< (car x) (car y)))))

(defun get-weather (place-name &key (api-key "b37f3e99-cdb8-4858-b850-bfffea6542f9")
		    (time-step 30))
  (let* ((result (get-weather-data place-name api-key time-step))
	 (locations (car (third result)))
	 (observations (car (fourth result))))
    (list
     (car (first result))
     (car (second result))
     (sort-temperature-tuples
      (temperature-tuples
       locations observations)))))
