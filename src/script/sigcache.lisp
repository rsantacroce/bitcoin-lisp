(defpackage #:bitcoin-lisp-sigcache
  (:use :cl)
  (:export #:sigcache
           #:add-to-sigcache
           #:check-sigcache))
(in-package :bitcoin-lisp-sigcache)

(defstruct sigcache
  (cache (make-hash-table :test #'equal) :type hash-table))

(defun add-to-sigcache (signature pubkey cache)
  ;; TODO: Implement adding signature to the sigcache.
  )

(defun check-sigcache (signature pubkey cache)
  ;; TODO: Implement checking signature in the sigcache.
  )
