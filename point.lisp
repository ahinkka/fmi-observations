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

(defclass point2d (fare-mop:simple-print-object-mixin)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defun point2d-equal (a b)
  (and
   (= (x a) (x b))
   (= (y a) (y b))))
(defun point2d= (a b) (point2d-equal a b))

(defun make-point2d (&key x y)
  (check-type x number)
  (check-type y number)
  (make-instance 'point2d :x x :y y))
