(asdf:defsystem #:bitcoin-lisp
  :serial t
  :description "An honest attemp to implement the bitcoin core code in lisp using as much as open soure as possible."
  :author "Roberto Santacroce <rob@santacroce.xyz>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:ironclad
               #:iterate)
  :components ((:file "src/consensus/consensus")
               (:file "src/consensus/params")
               (:file "src/consensus/validation")
               (:file "src/consensus/tx-verify")
               (:file "src/script/script")
               (:file "src/script/interpreter")
               (:file "src/script/sigcache")
               (:file "src/script/standard")
               (:file "src/primitives/transaction")
               (:file "src/primitives/block")
               (:file "src/chainparams")
               (:file "src/pow")
               (:file "src/validation")
               (:file "src/utils/crypto")
               (:file "src/utils/hash")
               (:file "src/utils/endian")
               (:file "src/main"))
  :in-order-to ((test-op (load-op #:bitcoin-lisp-tests))))

(asdf:defsystem #:bitcoin-lisp-tests
  :serial t
  :description "An honest attemp to implement the bitcoin core code in lisp using as much as open soure as possible."
  :author "Roberto Santacroce <rob@santacroce.xyz>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:bitcoin-lisp
               #:rove)
  :components ((:file "tests/consensus-tests")
               (:file "tests/script-tests")
               (:file "tests/primitives-tests")
               (:file "tests/chainparams-tests")
               (:file "tests/pow-tests")
               (:file "tests/validation-tests")
               (:file "tests/utils-tests")))
