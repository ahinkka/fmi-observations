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
  (:export :observations
	   :station-observations

	   :observation-time
	   :station-region
	   :station-location
	   :temperature
	   :windspeed
	   :wind-gusts
	   :wind-direction
	   :rh
	   :td
	   :r-1h
	   :ri-10min
	   :snow-aws
	   :p-sea
	   :n-man
	   :visibility

	   :*location-to-fmisid*))

(in-package :fmi-observations)

(defvar *location-to-fmisid*
  '(((61.5358 28.1919) "150168") ((69.0714 27.4964) "129963")
    ((69.7558 27.0121) "126737") ((64.2172 27.7745) "126736")
    ((69.5817 28.8992) "102055") ((68.8489 28.3039) "102052")
    ((69.1403 27.27) "102047") ((69.0714 27.4964) "102042")
    ((69.7558 27.0121) "102035") ((68.6073 27.4181) "102033")
    ((68.9024 25.7408) "102026") ((68.6025 23.5803) "102019")
    ((69.0422 20.8578) "102017") ((69.0486 20.7956) "102016")
    ((67.7475 29.615) "102012") ((68.4774 28.3054) "102009")
    ((68.4321 27.4489) "102006") ((68.4145 27.4174) "102005")
    ((68.0838 27.1897) "102001") ((67.8209 27.7504) "102000")
    ((68.1693 25.7866) "101994") ((67.6516 24.9058) "101990")
    ((67.9911 24.2436) "101987") ((67.6955 24.8624) "101986")
    ((67.9911 24.2436) "101985") ((67.9728 24.1202) "101983")
    ((68.0619 24.0372) "101982") ((67.7842 24.8572) "101981")
    ((67.9684 23.6802) "101969") ((67.1625 29.1814) "101966")
    ((66.8333 28.6781) "101959") ((67.0215 27.2223) "101958")
    ((67.2854 28.1801) "101952") ((66.7152 27.1642) "101950")
    ((66.5789 26.0149) "101933") ((67.3661 26.6331) "101932")
    ((66.498 25.7122) "101928") ((66.5584 25.8354) "101920")
    ((66.7724 23.9672) "101914") ((66.5289 24.6533) "101908")
    ((66.1652 29.1552) "101897") ((66.3682 29.3191) "101887")
    ((65.9951 29.2261) "101886") ((65.5739 28.2481) "101885")
    ((65.9764 26.3714) "101873") ((65.8467 24.1775) "101851")
    ((65.6725 24.5192) "101846") ((65.7833 24.5833) "101840")
    ((64.6636 28.0603) "101831") ((64.9308 28.7502) "101826")
    ((65.3994 26.9669) "101805") ((64.4915 26.474) "101800")
    ((64.9375 25.3756) "101799") ((65.0058 25.3969) "101794")
    ((64.6836 25.0925) "101787") ((64.6656 24.4108) "101785")
    ((65.0396 24.5653) "101784") ((65.385 24.1) "101783")
    ((65.0193 24.7314) "101776") ((64.6117 23.9) "101775")
    ((64.1111 28.3422) "101756") ((63.6669 28.8326) "101743")
    ((63.8408 27.2241) "101726") ((64.2815 27.6786) "101725")
    ((63.7357 25.7092) "101705") ((64.1418 25.4282) "101695")
    ((64.0499 24.7278) "101690") ((63.8211 24.1672) "101689")
    ((63.8442 23.1281) "101675") ((64.3307 23.4503) "101673")
    ((63.7173 23.1424) "101662") ((63.9506 22.8481) "101661")
    ((63.7512 22.5266) "101660") ((62.6014 29.7267) "101632")
    ((62.5458 29.6717) "101628") ((63.2318 29.2377) "101609")
    ((62.6596 29.6147) "101608") ((63.3777 28.665) "101603")
    ((62.8923 27.6368) "101586") ((62.7986 27.9083) "101580")
    ((63.1434 27.3161) "101572") ((63.0081 27.8025) "101570")
    ((62.9215 26.4281) "101555") ((63.0819 25.862) "101537")
    ((62.51 24.8136) "101536") ((63.0887 24.2645) "101533")
    ((63.4454 24.4478) "101528") ((62.5347 24.2208) "101520")
    ((63.12 23.0472) "101519") ((63.1202 23.0473) "101503")
    ((62.9368 22.491) "101486") ((63.0986 21.6432) "101485")
    ((62.9783 20.7436) "101481") ((62.9347 21.1885) "101479")
    ((63.4353 21.0683) "101464") ((61.8017 29.3183) "101441")
    ((62.0628 28.5692) "101436") ((61.5358 28.1919) "101435")
    ((61.9461 28.9347) "101430") ((62.3219 27.9111) "101421")
    ((62.1736 27.8636) "101420") ((61.8921 27.8883) "101418")
    ((61.6872 27.2099) "101398") ((61.8792 26.0989) "101367")
    ((61.7042 25.5085) "101362") ((62.3976 25.6709) "101339")
    ((61.8594 24.8183) "101338") ((61.8455 24.2906) "101317")
    ((61.8564 24.7892) "101315") ((61.5174 23.7571) "101311")
    ((62.3276 23.5457) "101310") ((61.8383 22.4672) "101291")
    ((62.4133 22.1867) "101289") ((62.1905 22.7071) "101272")
    ((62.2033 21.1733) "101268") ((61.6303 21.3794) "101267")
    ((61.4444 29.4639) "101254") ((61.1978 28.4758) "101252")
    ((61.0573 28.2118) "101247") ((61.0394 28.5658) "101246")
    ((61.0439 28.1541) "101237") ((60.5271 27.6758) "101231")
    ((60.8903 26.9444) "101219") ((61.1998 26.0527) "101196")
    ((60.6964 26.8136) "101194") ((60.8903 26.9442) "101191")
    ((61.2164 25.1361) "101189") ((61.2649 25.5234) "101185")
    ((61.0537 25.0421) "101154") ((60.9619 25.6341) "101152")
    ((61.1147 24.3294) "101151") ((60.9992 24.4947) "101150")
    ((60.5086 24.6569) "101149") ((60.5958 24.8064) "101130")
    ((60.6465 23.8059) "101128") ((61.4655 23.7499) "101124")
    ((61.4206 23.6233) "101118") ((60.8139 23.5009) "101104")
    ((61.2528 22.3496) "101103") ((61.4789 21.7864) "101064")
    ((61.1447 21.3062) "101061") ((60.7222 21.0301) "101059")
    ((60.2864 27.1883) "101042") ((60.2747 26.4477) "101039")
    ((60.3752 26.9616) "101030") ((60.3914 25.6114) "101028")
    ((60.2036 25.6286) "101023") ((60.1715 24.9478) "101007")
    ((60.2076 24.7444) "101005") ((60.2029 24.9645) "101004")
    ((60.1047 24.9786) "100996") ((60.4186 24.4017) "100976")
    ((60.2439 24.0525) "100974") ((60.1751 24.9478) "100971")
    ((60.3272 24.9603) "100968") ((60.4633 23.6525) "100967")
    ((60.3736 23.1161) "100955") ((60.1714 22.7608) "100951")
    ((60.4544 22.1817) "100949") ((60.3778 22.0997) "100947")
    ((60.3867 22.5547) "100934") ((60.2581 20.7506) "100928")
    ((60.1116 21.7027) "100924")))


(defclass weather-observation ()
  ((name :accessor observation-time
         :initform (error "observation-time is required!")
         :initarg :observation-time)

   (station-region :accessor station-region :initarg :station-region)
   (station-location :accessor station-location :initarg :station-location)

   (temperature    :accessor temperature    :initarg :t2m)
   (windspeed      :accessor windspeed      :initarg :ws-10min)
   (wind-gusts     :accessor wind-gusts     :initarg :wg-10min)
   (wind-direction :accessor wind-direction :initarg :wd-10min)
   (rh             :accessor rh             :initarg :rh)
   (td             :accessor td             :initarg :td)
   (r-1h           :accessor r-1h           :initarg :r-1h)
   (ri-10min       :accessor ri-10min       :initarg :ri-10min)
   (snow-aws       :accessor snow-aws       :initarg :snow-aws)
   (p-sea          :accessor p-sea          :initarg :p-sea)
   (n-man          :accessor n-man          :initarg :n-man)
   (visibility     :accessor visibility     :initarg :vis)
   ))

(defmethod print-object ((object weather-observation) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (observation-time object))))

;;;
;;; Utility functions
;;;
(defun string-to-dom (string)
  (cxml:parse-octets
   (babel:string-to-octets string)
   (cxml-dom:make-dom-builder)))

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
      '|NaN|
      (parse-number:parse-number input)))

(defun line-to-number-parts (line)
  (mapcar #'error-ignoring-parse-number
	  (remove-if #'string-empty-p
		     (mapcar #'string-trim-spaces
			     (cl-ppcre:split " " line)))))

(defun symbol-position (list symbol)
  "Wrapper around POSITION that normalizes inputs to lowercase dash-separated form.
   Also works with strings as members of either parameter.

   For example:
     (symbol-position '(a b c d e_f) 'e-f) => 4"
  (flet ((format-lowercase (input)
	   (format nil "~(~a~)" input))
	 (normalize-delimiters (input)
	   (substitute #\- #\_ input)))
    (let ((string-list (mapcar (alexandria:compose #'normalize-delimiters #'format-lowercase) list))
	  (symbol-string (normalize-delimiters (format-lowercase symbol))))
      (position symbol-string string-list :test #'string=))))

(defun make-keyword (keyword-name)
  (alexandria:make-keyword (substitute #\- #\_ (string-upcase keyword-name))))

(defmacro ordered-list-key-lookup-helper (key key-list value-list)
  "(ordered-list-key-lookup-helper 'e-f '(a b c d e_f) '(1 2 3 4 5)) => (:E-F 5)"
  `(list
    (make-keyword ,key)
    (elt ,value-list (symbol-position ,key-list ,key))))


;;;
;;; Domain
;;;
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

(defun get-weather-data (&key place-name station-fmisid api-key time-step)
  (let* ((url
	  (cond
	    ((and place-name station-fmisid)
	     (error "place-name and station-fmisid are mutually exclusive parameters"))
	    ((not (eq nil place-name))
	     (format nil "http://data.fmi.fi/fmi-apikey/~A/wfs?request=getFeature&storedquery_id=fmi::observations::weather::multipointcoverage&place=~A&timestep=~A"
		      api-key (drakma:url-encode place-name :utf-8) time-step))
	    ((not (eq nil station-fmisid))
	     (format nil "http://data.fmi.fi/fmi-apikey/~A/wfs?request=getFeature&storedquery_id=fmi::observations::weather::multipointcoverage&fmisid=~A&timestep=~A"
		      api-key (drakma:url-encode station-fmisid :utf-8) time-step))))
	    (xml (drakma:http-request url :external-format-out :utf-8 :external-format-in :utf-8))
	    (dom (string-to-dom xml)))
    (list
     (extract-node-values dom "//target:region[@codeSpace='http://xml.fmi.fi/namespace/location/region']")
     (extract-node-values dom "//gml:name[@codeSpace='http://xml.fmi.fi/namespace/locationcode/name']")
     (extract-node-values dom "//gmlcov:positions")
     (extract-node-values dom "//gml:doubleOrNilReasonTupleList")
     (observation-fields dom))))

(defun collect-observations (station-region station-location locations observations attributes)
  (let ((locations-list (mapcar #'line-to-parts (string-to-lines locations)))
	(observations-list (mapcar #'line-to-number-parts (string-to-lines observations))))

    (loop
       for location in locations-list
       for observation in observations-list
       collecting
	 (flet
	     ((make-observation (time attributes)
		(apply #'make-instance
		       (concatenate 'list
				    (list 'weather-observation
					  :observation-time time
					  :station-region station-region
					  :station-location station-location)
				    attributes))))
	   (let ((attributes-applicable
		  (alexandria:flatten
		   (mapcar #'(lambda (attribute)
			       (ordered-list-key-lookup-helper attribute attributes observation))
			   attributes))))

	     (make-observation (local-time:unix-to-timestamp
				(parse-integer (car (last  location)))) attributes-applicable))))))

(defun weather-observation-temporal-comparator (x y)
  (local-time:timestamp< (observation-time x) (observation-time y)))

(defun station-observations (station-fmisid
			     &key (api-key "b37f3e99-cdb8-4858-b850-bfffea6542f9") (time-step 30))

  (let* ((result (get-weather-data :station-fmisid station-fmisid :api-key api-key :time-step time-step))
	 (station-region (car (first result)))
	 (station-location (car (second result)))
	 (locations (car (third result)))
	 (observations (car (fourth result)))
	 (attributes (fifth result)))
    (values
     (sort
      (collect-observations station-region station-location
			    locations observations attributes)
      #'weather-observation-temporal-comparator)
     station-region
     station-location)))

(defun observations (place-name &key (api-key "b37f3e99-cdb8-4858-b850-bfffea6542f9")
		    (time-step 30))
  (let* ((result (get-weather-data :place-name place-name :api-key api-key :time-step time-step))
	 (station-region (car (first result)))
	 (station-location (car (second result)))
	 (locations (car (third result)))
	 (observations (car (fourth result)))
	 (attributes (fifth result)))
    (values
     (sort
      (collect-observations station-region station-location
			    locations observations attributes)
      #'weather-observation-temporal-comparator)
     station-region
     station-location)))
    
