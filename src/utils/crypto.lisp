(defpackage #:bitcoin-lisp-utils-crypto
  (:use :cl)
  (:export #:ecdsa-verify))
(in-package :bitcoin-lisp-utils-crypto)

(defun ecdsa-verify (message signature public-key)
  "Verify an ECDSA signature using the given message, signature, and public key."
  (ironclad:verify-signature :secp256k1
                             (ironclad:make-digest :sha256)
                             message
                             signature
                             public-key))
