;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FMI-OBSERVATIONS; Base: 10 -*-
;;;; Copyright (c) 2013-2019, Atte Hinkka <atte.hinkka@iki.fi>
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

(in-package #:fmi-observations)

;; local-time's rfc3339 without nsecs
(defvar +fmi-iso-format+
  '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) :GMT-OFFSET-OR-Z))

(defclass weather-observation ()
  ((observation-time         :accessor observation-time
			     :initform (error "observation-time is required!")
			     :initarg :observation-time)

   (station                  :accessor station                  :initarg  :station)

   (temperature              :accessor temperature              :initarg  :t2m)
   (windspeed                :accessor windspeed                :initarg  :ws-10min)
   (wind-gusts               :accessor wind-gusts               :initarg  :wg-10min)
   (wind-direction           :accessor wind-direction           :initarg  :wd-10min)
   (relative-humidity        :accessor relative-humidity        :initarg  :rh)
   (dew-point                :accessor dew-point                :initarg  :td)
   (rain-1h                  :accessor rain-1h                  :initarg  :r-1h)
   (rain-10min               :accessor rain-10min               :initarg  :ri-10min)
   (snow-aws                 :accessor snow-aws                 :initarg  :snow-aws)
   (pressure-sealevel        :accessor pressure-sealevel        :initarg  :p-sea)
   (n-man                    :accessor n-man                    :initarg  :n-man)
   (visibility               :accessor visibility               :initarg  :vis)
   (weather-description-code :accessor weather-description-code :initarg  :wawa)))

(defmethod print-object ((object weather-observation) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (observation-time object))))

(defclass weather-station (fare-mop:simple-print-object-mixin)
  ((station-name     :accessor station-name     :initarg :station-name)
   (station-region   :accessor station-region   :initarg :station-region)
   (station-fmi-id   :accessor station-fmi-id   :initarg :station-fmi-id)
   (station-location :accessor station-location :initarg :station-location)))

(defgeneric criterion-as-pair (criterion)
  (:documentation "Generic method for turning search criteria into applicable GET parameter pairs."))

(define-condition fmi-observations-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to FMI-OBSERVATIONS."))

(define-condition http-request-condition (fmi-observations-condition error)
  ((status-code :initarg :status-code :initform (error "HTTP status must be defined.")
		:reader status-code)
   (response-body :initarg :response-body :initform nil	:reader response-body)
   (reason :initarg :reason :initform nil :reader reason))
  (:report (lambda (condition stream)
	     (format stream
		     "Received HTTP response with status code ~A. Reason: ~A"
		     (status-code condition) (reason condition)))))

(define-condition remote-error (http-request-condition) ())

(define-condition station-not-found-error (remote-error) ())

(define-condition no-observations-for-station-error (fmi-observations-condition error) ())


;;;
;;; Utility functions
;;;
(defun string-to-dom (string)
  (cxml:parse-octets
   (babel:string-to-octets string)
   (cxml-dom:make-dom-builder)))
  ;; (etypecase string
  ;;   (string (cxml:parse-octets (babel:string-to-octets string) (cxml-dom:make-dom-builder)))
  ;;   ((simple-array (unsigned-byte 8)) (cxml:parse-octets string (cxml-dom:make-dom-builder)))))

(defun string-empty-p (string)
  (= 0 (length string)))

(defun string-trim-spaces (string)
  (string-trim " " string))

(defun string-to-lines (string)
  "Splits argument string to a list of non-empty lines, ends trimmed."
  (remove-if #'string-empty-p
	     (mapcar #'string-trim-spaces
		     (cl-ppcre:split "\\n" string))))

(defun line-to-parts (line)
  (remove-if #'string-empty-p
	     (mapcar #'string-trim-spaces
		     (cl-ppcre:split " " line))))

(defun error-ignoring-parse-number (input)
  (if (string= input "NaN")
      nil
      (parse-number:parse-number input)))

(defun line-to-number-parts (line)
  (mapcar #'error-ignoring-parse-number
	  (remove-if #'string-empty-p
		     (mapcar #'string-trim-spaces
			     (cl-ppcre:split " " line)))))

(defun make-keyword (keyword-name)
  (alexandria:make-keyword (substitute #\- #\_ (string-upcase keyword-name))))

;; namespace-pair is actually a proper list, like (list "foo" "http://url/of/foo")
(defmacro extract-single-string-value (context path-expression namespace-pair)
  `(progn
     (assert (not (xpath:node-set-p ,context)))

     (let* ((node-set (xpath:with-namespaces (,namespace-pair)
			(xpath:evaluate ,path-expression ,context)))
	    (node-list (xpath:all-nodes node-set)))

       (assert (= (length node-list) 1))

       (xpath:string-value (first node-list)))))


;;;
;;; Domain
;;;
(defparameter *wfs-url* "http://opendata.fmi.fi/wfs")

(defun extract-locations (dom)
  (let ((node-set
	 (xpath:with-namespaces (("om" "http://www.opengis.net/om/2.0")
				 ("target" "http://xml.fmi.fi/namespace/om/atmosphericfeatures/1.0"))
	   (xpath:evaluate "//om:featureOfInterest//target:Location" dom))))
    (xpath:all-nodes node-set)))

(defun extract-point-xref (location-context)
  (xpath:with-namespaces (("target" "http://xml.fmi.fi/namespace/om/atmosphericfeatures/1.0")
			  ("xlink" "http://www.w3.org/1999/xlink"))
    (let* ((node-set (xpath:evaluate "target:representativePoint/@xlink:href"
				     location-context))
	   (node-list (xpath:all-nodes node-set)))
      (assert (= 1 (length node-list)))
      (xpath:string-value (first node-list)))))

(defun position-for-xref (dom xref)
  (let ((node-set
	 (xpath:with-namespaces (("gml" "http://www.opengis.net/gml/3.2"))
	   (xpath:evaluate
	    (format nil "//gml:Point[@gml:id=\"~A\"]/gml:pos"
		    (string-trim "#" xref))
	    dom))))
    (assert (not (xpath:node-set-empty-p node-set)))
    (xpath:string-value (xpath:first-node node-set))))

(defun extract-station-geolocation (location-context)
  (let* ((xref (extract-point-xref location-context))
	 (pos (position-for-xref location-context xref))
	 (parts (cl-ppcre:split " " pos)))
    (assert (= (length parts) 2))

    (let ((x (parse-number:parse-number (second parts)))
	  (y (parse-number:parse-number (first parts))))
      (make-point2d :x x :y y))))


(defmacro .extract-station-name. (context)
  `(extract-single-string-value ,context
    "gml:name[@codeSpace=\"http://xml.fmi.fi/namespace/locationcode/name\"]"
    ("gml" "http://www.opengis.net/gml/3.2")))

(defun extract-station-name (context) (.extract-station-name. context))


(defmacro .extract-station-region. (context)
  `(extract-single-string-value ,context
    "target:region[@codeSpace=\"http://xml.fmi.fi/namespace/location/region\"]"
    ("target" "http://xml.fmi.fi/namespace/om/atmosphericfeatures/1.0")))

(defun extract-station-region (context) (.extract-station-region. context))


(defmacro .extract-fmi-station-id. (context)
  `(extract-single-string-value ,context
    "gml:identifier[@codeSpace=\"http://xml.fmi.fi/namespace/stationcode/fmisid\"]"
    ("gml" "http://www.opengis.net/gml/3.2")))

(defun extract-fmi-station-id (context) (.extract-fmi-station-id. context))


(defun extract-stations (dom)
  (mapcar #'(lambda (location-node)
	      (make-instance 'weather-station
			     :station-name (extract-station-name location-node)
			     :station-region (extract-station-region location-node)
			     :station-fmi-id (extract-fmi-station-id location-node)
			     :station-location (extract-station-geolocation location-node)))
	  (extract-locations dom)))

(defun observation-fields (dom)
  (let (result)
    (xpath:with-namespaces (("swe" "http://www.opengis.net/swe/2.0"))
      (xpath:do-node-set (node (xpath:evaluate "//swe:DataRecord/swe:field" dom))
	(push (format nil "~A" (xpath:evaluate "string(@name)" node)) result)))
    (reverse result)))

(defun extract-node-values (dom xpath-expression)
  (xpath:with-namespaces (("gml" "http://www.opengis.net/gml/3.2")
			  ("target" "http://xml.fmi.fi/namespace/om/atmosphericfeatures/0.95")
			  ("gmlcov" "http://www.opengis.net/gmlcov/1.0"))
    (let ((node-set (xpath:evaluate xpath-expression dom)))
      (xpath:map-node-set->list
       #'(lambda (node) (xpath:evaluate "string()" node))
       node-set))))

(defmethod criterion-as-pair ((object place-name-criterion))
  (cons "place" (place-name object)))

(defmethod criterion-as-pair ((object bounding-box-criterion))
  (with-slots (bounding-box) object
    (with-slots (left-lower right-upper) bounding-box
      (cons "bbox"
            (format nil "~A,~A,~A,~A"
                    (x left-lower)
                    (y left-lower)
                    (x right-upper)
                    (y right-upper))))))

(defmethod criterion-as-pair ((object fmi-station-id-criterion))
  (cons "fmisid" (station-id object)))

(defun make-parameters (criterion time-step start-time end-time)
  (list
   '("request" . "getFeature")
   '("storedquery_id" . "fmi::observations::weather::multipointcoverage")
   '("projection" . "epsg:4326")
   (criterion-as-pair criterion)
   (cons "timestep" (format nil "~A" time-step))
   (cons "starttime" (local-time:format-timestring nil start-time :format +fmi-iso-format+))
   (cons "endtime" (local-time:format-timestring nil end-time :format +fmi-iso-format+))))

(defun get-weather-data (station-criterion &key time-step start-time end-time)
  (let ((url *wfs-url*)
	(parameters (make-parameters station-criterion time-step start-time end-time)))
    ;; (break url)
    ;; (break parameters)
    (multiple-value-bind (response-body status-code headers uri stream must-close reason-phrase)
	(drakma:http-request url :parameters parameters :external-format-out :utf-8 :external-format-in :utf-8)
      (declare (ignore headers uri stream must-close))

      ;; (break response-body)

      (unless (= status-code 200)
	(if (search "no locations found for place" response-body)
	    (error 'station-not-found-error :status-code status-code
		   :reason (format nil "No locations found for ~A." station-criterion))
	    (error 'remote-error :status-code status-code :reason reason-phrase
		   :response-body response-body)))

      ;; (break response-body)

      (let ((dom (string-to-dom response-body)))
	(list
	 (extract-stations dom)
	 (extract-node-values dom "//gmlcov:positions")
	 (extract-node-values dom "//gml:doubleOrNilReasonTupleList")
	 (observation-fields dom)
	 url
	 parameters)))))

(defun location-to-place-caches (dom)
  (flet
      ((trim-point-prefix (s) (string-trim "point-" s))
       (trim-spaces (s) (string-trim " " s)))

    (let*
	((station-alist
	  (mapcar #'list
		  (extract-node-values dom "//gml:identifier[@codeSpace='http://xml.fmi.fi/namespace/stationcode/fmisid']")
		  (extract-node-values dom "//target:region[@codeSpace='http://xml.fmi.fi/namespace/location/region']")
		  (extract-node-values dom "//gml:name[@codeSpace='http://xml.fmi.fi/namespace/locationcode/name']")))

	 (id-to-station (make-hash-table :test 'equal :size (length station-alist)))

	 (locations-alist
	  (mapcar #'cons
		  (mapcar #'trim-point-prefix
			  (extract-node-values dom "//gml:Point[@srsName='http://www.opengis.net/def/crs/EPSG/0/4258']/@gml:id"))
		  (mapcar #'trim-spaces
			  (extract-node-values
			   dom "//gml:Point[@srsName='http://www.opengis.net/def/crs/EPSG/0/4258']/gml:pos"))))
	 (location-to-id (make-hash-table :test 'equal :size (length locations-alist))))

      (iter (for pair in station-alist)
	    (setf (gethash (car pair) id-to-station) (cdr pair)))
      
      (iter (for pair in locations-alist)
	    (setf (gethash (cdr pair) location-to-id) (car pair)))

      (values
       location-to-id
       id-to-station))))

(defun collect-observations (location-to-station locations-and-times observations attributes)
  (let ((locations-list (mapcar #'line-to-parts (string-to-lines locations-and-times)))
	(observations-list (mapcar #'line-to-number-parts (string-to-lines observations)))
	(keyword-attributes (mapcar #'make-keyword attributes)))
    (assert (not (null location-to-station)))

    (loop
       for location in locations-list
       for observation in observations-list
       collecting
	 (flet
	     ((make-observation (time station attributes values)
		(let*
		    ((beginning (list 'weather-observation :observation-time time :station station))
		     (end (apply #'concatenate 'list (mapcar #'list attributes values)))
		     (args (concatenate 'list beginning end)))
		  (apply #'make-instance args))))

	   (let* ((point (make-point2d :x (parse-number:parse-number (cadr location))
				       :y (parse-number:parse-number (car location))))
		  (station (cdar (remove-if-not #'(lambda (pair) (point2d-equal point (car pair))) location-to-station))))

	     (assert (not (null station)) (point location-to-station)
		     "Key ~A not found from ~A" point location-to-station)

	     (make-observation (local-time:unix-to-timestamp (parse-integer (car (last location))))
			       station
			       keyword-attributes observation))))))

(defun weather-observation-temporal-comparator (x y)
  (local-time:timestamp< (observation-time x) (observation-time y)))

(defun observations (station-criterion &key (time-step 60)
					 (start-time (local-time:timestamp- (local-time:now) 1 :hour))
					 (end-time (local-time:now)))
  (check-type station-criterion criterion)
  (check-type time-step number)
  (check-type start-time local-time:timestamp)
  (check-type end-time local-time:timestamp)

  (let* ((result (get-weather-data station-criterion :time-step time-step :start-time start-time :end-time end-time))
	 (weather-stations (first result)))

    (when (= (length weather-stations) 0)
      (error 'no-observations-for-station-error
	     :format-control "No stations for criterion ~A."
	     :format-arguments (list station-criterion)))

    (let* ((locations-and-times (car (second result)))
	   (observations (car (third result)))
	   (attributes (fourth result))
	   (url (fifth result))
	   (parameters (sixth result))

	   (location-to-station (iterate (for station in weather-stations)
					 (collect (cons (station-location station) station) into stations-alist)
					 (finally (return stations-alist)))))
      (values
       (sort
	(collect-observations location-to-station locations-and-times observations attributes)
	#'weather-observation-temporal-comparator)
       url
       parameters))))
