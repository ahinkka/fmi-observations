;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FMI-OBSERVATIONS; Base: 10 -*-
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

(in-package #:fmi-observations)


(defclass criterion () ()
  (:documentation "Baseclass for geographical query criteria."))

(defclass place-name-criterion (criterion fare-mop:simple-print-object-mixin)
  ((place-name :accessor place-name :initarg :place-name)))

(defclass fmi-station-id-criterion (criterion fare-mop:simple-print-object-mixin)
  ((station-id :accessor station-id :initarg :station-id)))


;; Bounding box criterion
(defclass bounding-box (fare-mop:simple-print-object-mixin)
  ((left-lower  :accessor left-lower  :initarg :left-lower)
   (right-upper :accessor right-upper :initarg :right-upper)))

(defclass bounding-box-criterion (criterion fare-mop:simple-print-object-mixin)
  ((bounding-box :accessor bounding-box :initarg :bounding-box)))


;; Public helper functions for creating criteria
(defun make-bbox-criterion (min-x min-y max-x max-y)
  (check-type min-x number)
  (check-type min-y number)
  (check-type max-x number)
  (check-type max-y number)
  (make-instance
   'bounding-box-criterion
   :bounding-box
   (make-instance 'bounding-box
		  :left-lower (make-instance 'point2d
					     :x min-x
					     :y min-y)
		  :right-upper (make-instance 'point2d
					      :x max-x
					      :y max-y))))

(defun make-place-name-criterion (place-name)
  (check-type place-name string)
  (make-instance 'place-name-criterion :place-name place-name))

(defun make-fmisid-criterion (fmi-station-id)
  (check-type fmi-station-id integer)
  (make-instance 'fmi-station-id-criterion :station-id (format nil "~A" fmi-station-id)))
