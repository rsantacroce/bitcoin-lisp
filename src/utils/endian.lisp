(defpackage #:bitcoin-lisp-utils-endian
  (:use :cl)
  (:export #:swap-endianness))
(in-package :bitcoin-lisp-utils-endian)

(defun swap-endianness (data)
  "Swap the endianness of a string or byte sequence."
  (etypecase data
    (string (coerce (reverse (coerce data 'list)) 'string))
    (vector (reverse data))))
