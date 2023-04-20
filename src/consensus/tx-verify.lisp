(defpackage #:bitcoin-lisp-tx-verify
  (:use :cl)
  (:import-from #:bitcoin-lisp-primitives-transaction #:transaction)
  (:export #:check-transaction
           #:check-inputs-signatures))
(in-package :bitcoin-lisp-tx-verify)

(defun check-transaction (transaction)
  ;; TODO: Implement transaction checks.
  )

(defun check-inputs-signatures (transaction blockchain)
  ;; TODO: Implement input signature checks.
  )
