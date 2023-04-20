(defpackage #:bitcoin-lisp-primitives-transaction
  (:use :cl)
  (:export #:transaction
           #:transaction-input
           #:transaction-output))
(in-package :bitcoin-lisp-primitives-transaction)

;; Transaction input
(defstruct transaction-input
  (previous-output-hash "0" :type string)
  (previous-output-index 0 :type (integer 0))
  (signature-script "" :type string)
  (sequence 0 :type (integer 0)))

;; Transaction output
(defstruct transaction-output
  (value 0 :type (integer 0))
  (pk-script "" :type string))

;; Transaction
(defstruct transaction
  (version 0 :type (integer 0))
  (inputs nil :type (list transaction-input))
  (outputs nil :type (list transaction-output))
  (lock-time 0 :type (integer 0)))
