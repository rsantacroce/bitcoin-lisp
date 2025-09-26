(defpackage :bitcoin-crawler
  (:use :cl)
  (:export
   ;; Main entry points
   #:start-crawler
   #:stop-crawler
   #:sync-blockchain
   
   ;; Configuration
   #:load-config
   #:get-config
   #:set-config
   
   ;; Network operations
   #:connect-to-peers
   #:disconnect-all-peers
   #:get-peer-count
   
   ;; Database operations
   #:init-database
   #:close-database
   #:store-block
   #:get-block
   #:get-chain-tip
   #:get-block-height
   
   ;; Protocol messages
   #:make-version-message
   #:make-getblocks-message
   #:make-getdata-message
   #:parse-message
   #:serialize-message))

(defpackage :bitcoin-crawler.protocol
  (:use :cl)
  (:export
   ;; Crypto functions
   #:sha256
   #:double-sha256
   #:hash160
   
   ;; Serialization
   #:serialize-varint
   #:deserialize-varint
   #:serialize-varstr
   #:deserialize-varstr
   #:serialize-uint32
   #:deserialize-uint32
   #:serialize-uint64
   #:deserialize-uint64
   #:serialize-uint256
   #:deserialize-uint256
   #:serialize-uint160
   #:deserialize-uint160
   #:serialize-ipv6
   #:deserialize-ipv6
   
   ;; Message types
   #:version-message
   #:verack-message
   #:ping-message
   #:pong-message
   #:getblocks-message
   #:getdata-message
   #:inv-message
   #:block-message
   #:tx-message
   #:headers-message
   
   ;; Message creation
   #:make-version
   #:make-verack
   #:make-ping
   #:make-pong
   #:make-getblocks
   #:make-getdata
   #:make-inv
   #:make-block
   #:make-tx
   #:make-headers))

(defpackage :bitcoin-crawler.network
  (:use :cl)
  (:export
   ;; Connection management
   #:bitcoin-connection
   #:connect-to-peer
   #:disconnect-peer
   #:send-message
   #:receive-message
   #:connection-alive-p
   
   ;; Peer management
   #:bitcoin-peer
   #:peer-address
   #:peer-port
   #:peer-connection
   #:peer-last-seen
   #:peer-services
   #:peer-version
   #:peer-height
   #:add-peer
   #:remove-peer
   #:get-peer
   #:list-peers
   
   ;; Discovery
   #:discover-peers
   #:connect-to-seeds))

(defpackage :bitcoin-crawler.storage
  (:use :cl)
  (:export
   ;; Database operations
   #:bitcoin-database
   #:open-database
   #:close-database
   #:database-path
   #:database-open-p
   
   ;; Block operations
   #:store-block
   #:get-block
   #:block-exists-p
   #:get-block-height
   #:set-block-height
   #:get-chain-tip
   #:set-chain-tip
   
   ;; Transaction operations
   #:store-transaction
   #:get-transaction
   #:transaction-exists-p
   
   ;; Index operations
   #:store-block-index
   #:get-block-by-height
   #:get-block-hash-by-height))

(defpackage :bitcoin-crawler.sync
  (:use :cl)
  (:export
   ;; Synchronization
   #:bitcoin-sync-manager
   #:start-sync
   #:stop-sync
   #:sync-running-p
   #:get-sync-progress
   #:sync-to-height
   #:sync-to-tip))
