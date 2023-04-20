(defpackage #:bitcoin-lisp-params
  (:use :cl)
  (:export #:get-params))
(in-package :bitcoin-lisp-params)

(defun get-params (&optional (network "main"))
  "Get the chain parameters for the specified network (main, test, or regtest)."
  (cond ((string= network "main") (bitcoin-lisp-chainparams:mainnet-params))
        ((string= network "test") (bitcoin-lisp-chainparams:testnet-params))
        ((string= network "regtest") (bitcoin-lisp-chainparams:regtest-params))
        (t (error "Unknown network"))))
