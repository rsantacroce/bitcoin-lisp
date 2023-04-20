(defpackage #:bitcoin-lisp-script
  (:use :cl)
  (:import-from #:bitcoin-lisp-primitives-transaction #:transaction
                                                      #:transaction-input
                                                      #:transaction-output)
  (:export #:parse-script
           #:serialize-script))
(in-package :bitcoin-lisp-script)

(defun parse-script (raw-script)
  ;; TODO: Implement script parsing.
  )

(defun serialize-script (parsed-script)
  ;; TODO: Implement script serialization.
  )
