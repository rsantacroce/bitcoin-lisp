#!/usr/bin/env sbcl --script
;; Bitcoin Crawler Example Script
;; This script demonstrates how to use the Bitcoin crawler

(require :asdf)
(require :bitcoin-crawler)

(in-package :bitcoin-crawler)

(defun example-sync ()
  "Example: Sync blockchain to a specific height"
  (format t "Bitcoin Crawler Example~%")
  (format t "======================~%")
  (format t "~%")
  
  ;; Setup logging
  (setup-logging :level :info :file "./example.log")
  
  ;; Load configuration
  (load-config)
  
  ;; Set custom configuration
  (set-config "database-path" "./example-bitcoin-data")
  (set-config "max-peers" 4)
  (set-config "sync-batch-size" 100)
  
  (format t "Starting blockchain synchronization...~%")
  (format t "Database: ~A~%" (get-config "database-path"))
  (format t "Max peers: ~A~%" (get-config "max-peers"))
  (format t "~%")
  
  ;; Sync blockchain to height 1000 (for example)
  (let ((sync-manager (sync-blockchain 
                       :database-path (get-config "database-path")
                       :target-height 1000
                       :max-peers (parse-integer (get-config "max-peers")))))
    
    (format t "Synchronization completed!~%")
    (format t "~%")
    
    ;; Show final status
    (show-status)
    (format t "~%")
    
    ;; Show chain information
    (show-chain-info)
    (format t "~%")
    
    ;; Show some block information
    (let ((chain-info (get-chain-info)))
      (when chain-info
        (let ((tip-hash (getf chain-info :tip-hash)))
          (when tip-hash
            (let ((block-info (get-block-info (hex-to-bytes tip-hash))))
              (when block-info
                (format t "Latest Block Information:~%")
                (format t "  Hash: ~A~%" (getf block-info :hash))
                (format t "  Height: ~A~%" (getf block-info :height))
                (format t "  Size: ~A bytes~%" (getf block-info :size))
                (format t "  Weight: ~A~%" (getf block-info :weight))
                (format t "  Transactions: ~A~%" (getf block-info :tx-count))
                (format t "  Timestamp: ~A~%" (getf block-info :timestamp))
                (format t "  Bits: ~A~%" (getf block-info :bits))
                (format t "  Nonce: ~A~%" (getf block-info :nonce))))))))
    
    (format t "~%")
    (format t "Example completed successfully!~%")))

(defun example-interactive ()
  "Example: Interactive crawler session"
  (format t "Bitcoin Crawler Interactive Example~%")
  (format t "===================================~%")
  (format t "~%")
  
  ;; Setup logging
  (setup-logging :level :info :file "./interactive.log")
  
  ;; Load configuration
  (load-config)
  
  ;; Start the crawler
  (format t "Starting crawler...~%")
  (let ((crawler (start-crawler :database-path "./interactive-bitcoin-data" :max-peers 6)))
    
    (format t "Crawler started!~%")
    (format t "~%")
    
    ;; Let it run for a bit
    (format t "Letting crawler run for 30 seconds...~%")
    (sleep 30)
    
    ;; Show status
    (show-status)
    (format t "~%")
    
    ;; Show peers
    (show-peers)
    (format t "~%")
    
    ;; Stop the crawler
    (format t "Stopping crawler...~%")
    (stop-crawler)
    
    (format t "Interactive example completed!~%")))

(defun example-database-operations ()
  "Example: Direct database operations"
  (format t "Bitcoin Database Operations Example~%")
  (format t "===================================~%")
  (format t "~%")
  
  ;; Open database
  (let ((database (open-database "./example-db")))
    
    ;; Initialize with genesis block
    (initialize-database database)
    
    (format t "Database opened and initialized~%")
    
    ;; Get database statistics
    (let ((stats (get-database-stats database)))
      (format t "Database Statistics:~%")
      (format t "  Blocks: ~A~%" (getf stats :block-count))
      (format t "  Transactions: ~A~%" (getf stats :tx-count))
      (format t "  Total Size: ~A bytes~%" (getf stats :total-size))
      (format t "  Open: ~A~%" (getf stats :open-p)))
    
    (format t "~%")
    
    ;; Get chain information
    (let ((chain-state (get-chain-tip database)))
      (if chain-state
          (progn
            (format t "Chain State:~%")
            (format t "  Tip Hash: ~A~%" (bytes-to-hex (chain-state-tip-hash chain-state)))
            (format t "  Tip Height: ~A~%" (chain-state-tip-height chain-state))
            (format t "  Total Work: ~A~%" (chain-state-total-work chain-state))
            (format t "  Last Update: ~A~%" (chain-state-last-update chain-state)))
          (format t "No chain state available~%")))
    
    (format t "~%")
    
    ;; Close database
    (close-database database)
    (format t "Database closed~%")
    
    (format t "Database operations example completed!~%")))

;; Main execution
(defun main ()
  "Main function for the example script"
  (let ((example (or (first *posix-argv*) "sync")))
    (case (intern (string-upcase example) :keyword)
      (:sync (example-sync))
      (:interactive (example-interactive))
      (:database (example-database-operations))
      (t
       (format t "Usage: ~A [sync|interactive|database]~%" (first *posix-argv*))
       (format t "  sync       - Sync blockchain example~%")
       (format t "  interactive - Interactive crawler example~%")
       (format t "  database   - Database operations example~%")))))

;; Run the example
(main)
