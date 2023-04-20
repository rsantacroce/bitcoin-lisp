(defpackage #:bitcoin-lisp-utils-hash
  (:use :cl)
  (:export #:double-sha256))
(in-package :bitcoin-lisp-utils-hash)

(defun double-sha256 (data)
  "Calculate the double SHA-256 hash of the input data."
  (ironclad:digest-sequence :sha256
                            (ironclad:digest-sequence :sha256 data)))
