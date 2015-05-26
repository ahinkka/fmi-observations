;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; Copyright (c) 2015, Atte Hinkka <atte.hinkka@iki.fi>
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

(in-package :cl-user)

(defpackage #:fmi-observations
  (:use #:common-lisp #:iterate)
  (:export :observations
	   :*api-key*
	   :no-stations-error

	   ;; criteria makers
	   :make-bbox-criterion
	   :make-place-name-criterion
	   :make-fmisid-criterion

	   ;; point2d accessors
	   :x
	   :y

	   ;; weather-station accessors
	   :station-name
	   :station-region
	   :station-fmi-id
	   :station-location

	   ;; weather-observation accessors
	   :observation-time
	   :station

	   ;; weather-observation attribute accessors
	   :temperature
	   :windspeed
	   :wind-gusts
	   :wind-direction
	   :relative-humidity
	   :dew-point
	   :rain-1h
	   :rain-10min
	   :snow-aws
	   :pressure-sealevel
	   :n-man
	   :visibility
	   :weather-description-code))
