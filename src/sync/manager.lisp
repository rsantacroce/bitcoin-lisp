(in-package :bitcoin-crawler.sync)

;; Bitcoin synchronization manager

(defvar *sync-manager* nil)

(defun start-crawler (&key (database-path "./bitcoin-data") (max-peers 8))
  "Start the Bitcoin crawler"
  (log:info "Starting Bitcoin crawler...")
  
  ;; Initialize database
  (let ((database (open-database database-path)))
    (initialize-database database)
    
    ;; Start peer discovery
    (connect-to-seeds :max-connections max-peers)
    (start-peer-manager)
    
    ;; Start synchronization
    (let ((sync-manager (start-sync database)))
      (setf *sync-manager* sync-manager)
      
      (log:info "Bitcoin crawler started successfully")
      (log:info "Database: ~A" database-path)
      (log:info "Max peers: ~A" max-peers)
      
      sync-manager)))

(defun stop-crawler ()
  "Stop the Bitcoin crawler"
  (log:info "Stopping Bitcoin crawler...")
  
  ;; Stop synchronization
  (when *sync-manager*
    (stop-sync *sync-manager*)
    (setf *sync-manager* nil))
  
  ;; Stop peer management
  (stop-peer-manager)
  
  ;; Close database
  (when *sync-manager*
    (close-database (sync-database *sync-manager*)))
  
  (log:info "Bitcoin crawler stopped"))

(defun sync-blockchain (&key (database-path "./bitcoin-data") (target-height 0) (max-peers 8))
  "Synchronize the blockchain"
  (let ((database (open-database database-path)))
    (initialize-database database)
    
    ;; Connect to peers
    (connect-to-seeds :max-connections max-peers)
    (start-peer-manager)
    
    ;; Wait for peers to connect
    (sleep 5)
    
    ;; Start synchronization
    (let ((sync-manager (if (> target-height 0)
                            (sync-to-height database target-height)
                            (sync-to-tip database))))
      
      ;; Stop peer management
      (stop-peer-manager)
      
      ;; Close database
      (close-database database)
      
      (log:info "Blockchain synchronization completed")
      sync-manager)))

(defun get-crawler-status ()
  "Get current crawler status"
  (if *sync-manager*
      (let ((progress (get-sync-progress *sync-manager*))
            (peer-stats (peer-stats)))
        (list :running-p t
              :sync-progress progress
              :peer-stats peer-stats
              :database-stats (get-database-stats (sync-database *sync-manager*))))
      (list :running-p nil)))

(defun get-sync-progress ()
  "Get synchronization progress"
  (if *sync-manager*
      (get-sync-progress *sync-manager*)
      (list :running-p nil)))

(defun pause-sync ()
  "Pause synchronization"
  (when *sync-manager*
    (setf (sync-running-p *sync-manager*) nil)
    (log:info "Synchronization paused")))

(defun resume-sync ()
  "Resume synchronization"
  (when *sync-manager*
    (setf (sync-running-p *sync-manager*) t)
    (log:info "Synchronization resumed")))

(defun get-block-info (block-hash)
  "Get information about a specific block"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*))
          (block-data (get-block database block-hash)))
      (when block-data
        (list :hash (bitcoin-crawler.protocol:bytes-to-hex block-hash)
              :height (block-data-height block-data)
              :size (block-data-size block-data)
              :weight (block-data-weight block-data)
              :tx-count (length (block-data-transactions block-data))
              :timestamp (block-timestamp (block-data-header block-data))
              :bits (block-bits (block-data-header block-data))
              :nonce (block-nonce (block-data-header block-data)))))))

(defun get-transaction-info (tx-hash)
  "Get information about a specific transaction"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*))
          (tx-data (get-transaction database tx-hash)))
      (when tx-data
        (list :hash (bitcoin-crawler.protocol:bytes-to-hex tx-hash)
              :version (tx-data-version tx-data)
              :input-count (length (tx-data-inputs tx-data))
              :output-count (length (tx-data-outputs tx-data))
              :size (tx-data-size tx-data)
              :weight (tx-data-weight tx-data)
              :fee (tx-data-fee tx-data)
              :locktime (tx-data-locktime tx-data)
              :confirmed-height (tx-data-confirmed-height tx-data))))))

(defun get-chain-info ()
  "Get blockchain information"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*))
          (chain-state (get-chain-tip database)))
      (when chain-state
        (list :tip-hash (bitcoin-crawler.protocol:bytes-to-hex (chain-state-tip-hash chain-state))
              :tip-height (chain-state-tip-height chain-state)
              :total-work (chain-state-total-work chain-state)
              :last-update (chain-state-last-update chain-state))))))

(defun search-blocks (height)
  "Search for blocks at a specific height"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*))
          (block-hash (get-block-by-height database height)))
      (when block-hash
        (get-block-info block-hash)))))

(defun search-transactions (tx-hash)
  "Search for a specific transaction"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*)))
      (get-transaction-info (bitcoin-crawler.protocol:hex-to-bytes tx-hash)))))

(defun export-blockchain (output-file &key (start-height 0) (end-height nil))
  "Export blockchain data to file"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*))
          (current-height (get-chain-tip-height database)))
      (let ((end-height (or end-height current-height)))
        (with-open-file (stream output-file :direction :output :if-exists :overwrite)
          (format stream "Bitcoin Blockchain Export~%")
          (format stream "Start Height: ~A~%" start-height)
          (format stream "End Height: ~A~%" end-height)
          (format stream "Export Time: ~A~%" (get-universal-time))
          (format stream "~%")
          
          (loop for height from start-height to end-height
                do (let ((block-hash (get-block-by-height database height)))
                     (when block-hash
                       (let ((block-info (get-block-info block-hash)))
                         (when block-info
                           (format stream "Block ~A: ~A~%" height (getf block-info :hash)))))))))))

(defun import-blockchain (input-file)
  "Import blockchain data from file"
  (log:info "Blockchain import not implemented yet"))

(defun validate-blockchain ()
  "Validate the entire blockchain"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*))
          (current-height (get-chain-tip-height database))
          (valid-blocks 0)
          (invalid-blocks 0))
      
      (log:info "Validating blockchain from height 0 to ~A" current-height)
      
      (loop for height from 0 to current-height
            do (let ((block-hash (get-block-by-height database height)))
                 (when block-hash
                   (let ((block-data (get-block database block-hash)))
                     (if (is-block-valid block-data)
                         (incf valid-blocks)
                         (progn
                           (incf invalid-blocks)
                           (log:warn "Invalid block at height ~A" height)))))))
      
      (log:info "Blockchain validation completed: ~A valid, ~A invalid" valid-blocks invalid-blocks)
      (list :valid-blocks valid-blocks
            :invalid-blocks invalid-blocks
            :total-blocks (+ valid-blocks invalid-blocks)))))

(defun cleanup-database ()
  "Clean up the database"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*)))
      (compact-database database)
      (log:info "Database cleanup completed"))))

(defun backup-database (backup-path)
  "Backup the database"
  (when *sync-manager*
    (let ((database (sync-database *sync-manager*)))
      (backup-database database backup-path)
      (log:info "Database backup completed"))))

;; Interactive commands
(defun show-status ()
  "Show current crawler status"
  (let ((status (get-crawler-status)))
    (format t "Bitcoin Crawler Status~%")
    (format t "====================~%")
    (format t "Running: ~A~%" (getf status :running-p))
    (when (getf status :running-p)
      (let ((progress (getf status :sync-progress)))
        (format t "Current Height: ~A~%" (getf progress :current-height))
        (format t "Target Height: ~A~%" (getf progress :target-height))
        (format t "Blocks Synced: ~A~%" (getf progress :blocks-synced))
        (format t "Progress: ~A%~%" (getf progress :progress-percent))
        (format t "Sync Rate: ~A blocks/sec~%" (getf progress :sync-rate))))
    (let ((peer-stats (getf status :peer-stats)))
      (format t "Peers: ~A connected (~A total)~%" 
              (getf peer-stats :connected) (getf peer-stats :total))
      (format t "Max Height: ~A~%" (getf peer-stats :max-height)))
    (let ((db-stats (getf status :database-stats)))
      (format t "Database: ~A blocks, ~A transactions~%" 
              (getf db-stats :block-count) (getf db-stats :tx-count))
      (format t "Total Size: ~A bytes~%" (getf db-stats :total-size)))))

(defun show-peers ()
  "Show connected peers"
  (let ((peers (get-peer-list)))
    (format t "Connected Peers~%")
    (format t "===============~%")
    (if peers
        (dolist (peer peers)
          (format t "~A:~A - Height: ~A, Version: ~A~%" 
                  (getf peer :address) (getf peer :port)
                  (getf peer :height) (getf peer :version)))
        (format t "No peers connected~%"))))

(defun show-chain-info ()
  "Show blockchain information"
  (let ((chain-info (get-chain-info)))
    (if chain-info
        (progn
          (format t "Blockchain Information~%")
          (format t "====================~%")
          (format t "Tip Hash: ~A~%" (getf chain-info :tip-hash))
          (format t "Tip Height: ~A~%" (getf chain-info :tip-height))
          (format t "Total Work: ~A~%" (getf chain-info :total-work))
          (format t "Last Update: ~A~%" (getf chain-info :last-update)))
        (format t "No blockchain information available~%"))))
