(defpackage #:bitcoin-lisp-interpreter
  (:use :cl)
  (:import-from #:bitcoin-lisp-primitives-transaction #:transaction
                                                      #:transaction-input
                                                      #:transaction-output)
  (:export #:interpret-script
           #:execute-script))
(in-package :bitcoin-lisp-interpreter)

(defun interpret-script (script input output)
  ;; TODO: Implement script interpretation.
  )

(defun execute-script (transaction-input transaction-output)
  ;; TODO: Implement script execution.
  )
