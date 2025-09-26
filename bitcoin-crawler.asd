(defsystem "bitcoin-crawler"
  :description "Bitcoin network crawler and blockchain synchronizer"
  :version "0.1.0"
  :author "Bitcoin Lisp Project"
  :license "MIT"
  :depends-on ("usocket" "nibbles" "crypto" "babel" "flexi-streams" "cl-ppcre" "log4cl")
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "src/protocol/crypto" :depends-on ("package"))
   (:file "src/protocol/serialization" :depends-on ("package" "src/protocol/crypto"))
   (:file "src/protocol/messages" :depends-on ("package" "src/protocol/serialization"))
   (:file "src/network/connection" :depends-on ("package" "src/protocol/messages"))
   (:file "src/network/peer" :depends-on ("package" "src/network/connection"))
   (:file "src/network/discovery" :depends-on ("package" "src/network/peer"))
   (:file "src/storage/schema" :depends-on ("package"))
   (:file "src/storage/database" :depends-on ("package" "src/storage/schema"))
   (:file "src/sync/blockchain" :depends-on ("package" "src/storage/database" "src/network/peer"))
   (:file "src/sync/manager" :depends-on ("package" "src/sync/blockchain"))
   (:file "main" :depends-on ("package" "config" "src/sync/manager"))))
