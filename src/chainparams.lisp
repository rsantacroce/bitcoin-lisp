(defpackage #:bitcoin-lisp-chainparams
  (:use :cl)
  (:export #:chain-params
           #:mainnet-params
           #:testnet-params
           #:regtest-params))
(in-package :bitcoin-lisp-chainparams)

(defstruct chain-params
  (name "" :type string)
  (genesis-block nil :type (or null block-header))
  (pow-target-timespan 0 :type (integer 0))
  (pow-target-spacing 0 :type (integer 0))
  (pow-retarget-interval 0 :type (integer 0))
  (subsidy-halving-interval 0 :type (integer 0))
  (base-bip16-height 0 :type (integer 0)))

(defvar *mainnet-params*
  (make-chain-params
    :name "main"
    :genesis-block *mainnet-genesis-block*
    :pow-target-timespan +target-timespan+
    :pow-target-spacing +target-spacing+
    :pow-retarget-interval +interval+
    :subsidy-halving-interval 210000
    :base-bip16-height 19795))

(defvar *testnet-params*
  (make-chain-params
    :name "test"
    ;; TODO: Add Testnet-specific parameters.
    ))

(defvar *regtest-params*
  (make-chain-params
    :name "regtest"
    ;; TODO: Add Regtest-specific parameters.
    ))
