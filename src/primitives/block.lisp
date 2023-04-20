(defpackage #:bitcoin-lisp-primitives-block
  (:use :cl)
  (:export #:block-header))
(in-package :bitcoin-lisp-primitives-block)

(defstruct block-header
  (version 0 :type (integer 0))
  (previous-block-hash "0" :type string)
  (merkle-root-hash "0" :type string)
  (timestamp 0 :type (integer 0))
  (bits 0 :type (integer 0))
  (nonce 0 :type (integer 0)))
