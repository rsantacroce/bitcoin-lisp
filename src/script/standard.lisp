(defpackage #:bitcoin-lisp-standard
  (:use :cl)
  (:import-from #:bitcoin-lisp-primitives-transaction #:transaction)
  (:export #:is-standard-transaction))
(in-package :bitcoin-lisp-standard)

(defun is-standard-transaction (transaction)
  ;; TODO: Implement standard transaction checks.
  )
