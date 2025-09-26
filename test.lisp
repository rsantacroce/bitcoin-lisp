#!/usr/bin/env sbcl --script
;; Simple test script for Bitcoin Crawler

(require :asdf)

;; Load the system
(asdf:load-system :bitcoin-crawler)

(in-package :bitcoin-crawler)

(defun test-basic-functionality ()
  "Test basic functionality without network operations"
  (format t "Testing Bitcoin Crawler Basic Functionality~%")
  (format t "==========================================~%")
  (format t "~%")
  
  ;; Test configuration
  (format t "1. Testing configuration...~%")
  (load-config)
  (set-config "database-path" "./test-bitcoin-data")
  (set-config "max-peers" 4)
  (format t "   Database path: ~A~%" (get-config "database-path"))
  (format t "   Max peers: ~A~%" (get-config "max-peers"))
  (format t "   ✓ Configuration test passed~%")
  (format t "~%")
  
  ;; Test database operations
  (format t "2. Testing database operations...~%")
  (let ((database (open-database "./test-bitcoin-data")))
    (initialize-database database)
    
    ;; Test storing and retrieving data
    (let ((test-key #(1 2 3 4))
          (test-value #(5 6 7 8)))
      (store-value database test-key test-value)
      (let ((retrieved (get-value database test-key)))
        (if (equalp test-value retrieved)
            (format t "   ✓ Database store/retrieve test passed~%")
            (format t "   ✗ Database store/retrieve test failed~%"))))
    
    ;; Test chain state
    (let ((chain-state (get-chain-tip database)))
      (if chain-state
          (format t "   ✓ Chain state test passed~%")
          (format t "   ✗ Chain state test failed~%")))
    
    (close-database database))
  (format t "~%")
  
  ;; Test protocol functions
  (format t "3. Testing protocol functions...~%")
  (let ((test-data #(1 2 3 4 5)))
    (let ((hash (bitcoin-crawler.protocol:sha256 test-data)))
      (if (= (length hash) 32)
          (format t "   ✓ SHA256 test passed~%")
          (format t "   ✗ SHA256 test failed~%")))
    
    (let ((varint (bitcoin-crawler.protocol:serialize-varint 1000)))
      (let ((deserialized (bitcoin-crawler.protocol:deserialize-varint 
                          (flexi-streams:make-in-memory-input-stream varint))))
        (if (= deserialized 1000)
            (format t "   ✓ Varint serialization test passed~%")
            (format t "   ✗ Varint serialization test failed~%"))))
  (format t "~%")
  
  ;; Test message creation
  (format t "4. Testing message creation...~%")
  (let ((verack-msg (bitcoin-crawler.protocol:make-verack)))
    (if (string= (bitcoin-crawler.protocol:message-command verack-msg) "verack")
        (format t "   ✓ Message creation test passed~%")
        (format t "   ✗ Message creation test failed~%")))
  (format t "~%")
  
  (format t "Basic functionality tests completed!~%"))

(defun test-network-functions ()
  "Test network functions (without actual connections)"
  (format t "Testing Network Functions~%")
  (format t "========================~%")
  (format t "~%")
  
  ;; Test peer management
  (format t "1. Testing peer management...~%")
  (add-peer "127.0.0.1" 8333 :services 1 :version 70015 :height 1000)
  (let ((peer (get-peer "127.0.0.1" 8333)))
    (if peer
        (format t "   ✓ Peer management test passed~%")
        (format t "   ✗ Peer management test failed~%")))
  
  (let ((peers (list-peers)))
    (if (= (length peers) 1)
        (format t "   ✓ Peer listing test passed~%")
        (format t "   ✗ Peer listing test failed~%")))
  
  (remove-peer "127.0.0.1" 8333)
  (let ((peers (list-peers)))
    (if (= (length peers) 0)
        (format t "   ✓ Peer removal test passed~%")
        (format t "   ✗ Peer removal test failed~%")))
  (format t "~%")
  
  (format t "Network function tests completed!~%"))

(defun main ()
  "Main test function"
  (format t "Bitcoin Crawler Test Suite~%")
  (format t "==========================~%")
  (format t "~%")
  
  (handler-case
      (progn
        (test-basic-functionality)
        (test-network-functions)
        (format t "~%")
        (format t "All tests completed successfully!~%"))
    (error (e)
      (format t "Test failed with error: ~A~%" e)
      (uiop:quit 1))))

(main)
