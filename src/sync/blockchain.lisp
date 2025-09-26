(in-package :bitcoin-crawler.sync)

;; Bitcoin blockchain synchronization

(defclass bitcoin-sync-manager ()
  ((database :initarg :database :accessor sync-database :type bitcoin-database)
   (running-p :initarg :running-p :accessor sync-running-p :type boolean :initform nil)
   (current-height :initarg :current-height :accessor sync-current-height :type (unsigned-byte 32) :initform 0)
   (target-height :initarg :target-height :accessor sync-target-height :type (unsigned-byte 32) :initform 0)
   (sync-thread :initarg :sync-thread :accessor sync-thread :type (or thread null) :initform nil)
   (last-update :initarg :last-update :accessor sync-last-update :type (unsigned-byte 32) :initform 0)
   (blocks-synced :initarg :blocks-synced :accessor sync-blocks-synced :type (unsigned-byte 32) :initform 0)
   (sync-start-time :initarg :sync-start-time :accessor sync-start-time :type (unsigned-byte 32) :initform 0)))

(defun start-sync (database &key (target-height 0))
  "Start blockchain synchronization"
  (let ((sync-manager (make-instance 'bitcoin-sync-manager
                                     :database database
                                     :target-height target-height)))
    (setf (sync-current-height sync-manager) (get-chain-tip-height database)
          (sync-start-time sync-manager) (get-universal-time)
          (sync-running-p sync-manager) t)
    
    (setf (sync-thread sync-manager)
          (bt:make-thread
           (lambda ()
             (sync-loop sync-manager))
           :name "bitcoin-sync"))
    
    (log:info "Started blockchain synchronization from height ~A" (sync-current-height sync-manager))
    sync-manager))

(defun stop-sync (sync-manager)
  "Stop blockchain synchronization"
  (setf (sync-running-p sync-manager) nil)
  (when (sync-thread sync-manager)
    (bt:destroy-thread (sync-thread sync-manager))
    (setf (sync-thread sync-manager) nil))
  (log:info "Stopped blockchain synchronization"))

(defun sync-loop (sync-manager)
  "Main synchronization loop"
  (handler-case
      (loop while (sync-running-p sync-manager)
            do (progn
                 (sync-next-batch sync-manager)
                 (sleep 1)))
    (error (e)
      (log:error "Error in sync loop: ~A" e)
      (setf (sync-running-p sync-manager) nil))))

(defun sync-next-batch (sync-manager)
  "Sync next batch of blocks"
  (let ((database (sync-database sync-manager))
        (current-height (sync-current-height sync-manager))
        (batch-size (get-config "sync-batch-size" 500)))
    
    ;; Get the best peer
    (let ((best-peer (get-best-peer)))
      (if best-peer
          (let ((peer-height (peer-height best-peer)))
            (if (> peer-height current-height)
                (progn
                  (log:info "Syncing blocks ~A to ~A" current-height (min (+ current-height batch-size) peer-height))
                  (sync-blocks-range sync-manager best-peer current-height (min (+ current-height batch-size) peer-height))
                  (setf (sync-current-height sync-manager) (min (+ current-height batch-size) peer-height))
                  (setf (sync-target-height sync-manager) peer-height))
                (progn
                  (log:info "Synchronized to peer height ~A" peer-height)
                  (setf (sync-running-p sync-manager) nil))))
          (progn
            (log:warn "No peers available for synchronization")
            (sleep 5))))))

(defun sync-blocks-range (sync-manager peer start-height end-height)
  "Sync a range of blocks from a peer"
  (let ((database (sync-database sync-manager))
        (address (peer-address peer))
        (port (peer-port peer)))
    
    ;; Request block headers first
    (let ((block-locator-hashes (get-block-locator-hashes database start-height))
          (hash-stop (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
      (request-headers-from-peer address port block-locator-hashes hash-stop))
    
    ;; Process incoming messages
    (let ((processed-blocks 0))
      (loop for height from start-height below end-height
            while (< processed-blocks (- end-height start-height))
            do (let ((message (receive-message-from-peer address port :timeout 30)))
                 (when message
                   (case (intern (string-upcase (bitcoin-crawler.protocol:message-command message)) :keyword)
                     (:block
                      (progn
                        (process-block-message database message)
                        (incf processed-blocks)
                        (incf (sync-blocks-synced sync-manager))))
                     (:headers
                      (process-headers-message database message))
                     (t
                      (log:debug "Received message: ~A" (bitcoin-crawler.protocol:message-command message))))))))))

(defun get-block-locator-hashes (database start-height)
  "Get block locator hashes for getblocks request"
  (let ((hashes '())
        (step 1)
        (height start-height))
    (loop while (and (>= height 0) (< (length hashes) 10))
          do (let ((block-hash (get-block-by-height database height)))
               (when block-hash
                 (push block-hash hashes))
               (setf height (- height step))
               (setf step (min (* step 2) 1000))))
    (reverse hashes)))

(defun process-block-message (database message)
  "Process a block message"
  (let ((payload (bitcoin-crawler.protocol:message-payload message)))
    (let ((stream (flexi-streams:make-in-memory-input-stream payload)))
      (let ((block-header (bitcoin-crawler.protocol:parse-block-header stream))
            (tx-count (bitcoin-crawler.protocol:deserialize-varint stream)))
        
        ;; Calculate block hash
        (let ((block-hash (bitcoin-crawler.protocol:bitcoin-hash (bitcoin-crawler.protocol:serialize-block-header block-header))))
          
          ;; Parse transactions
          (let ((transactions '()))
            (loop for i from 0 below tx-count
                  do (let ((tx (parse-transaction stream)))
                       (push tx transactions)))
            
            ;; Create block data
            (let ((block-data (make-block-data
                               :header block-header
                               :transactions (reverse transactions)
                               :size (length payload)
                               :stripped-size (length payload)
                               :weight (* (length payload) 4)
                               :height (get-block-height database block-hash)
                               :chain-work 1
                               :status :main)))
              
              ;; Store block
              (store-block database block-hash block-data)
              
              ;; Store block index
              (let ((height (get-block-height database block-hash)))
                (when height
                  (store-block-index database height block-hash)))
              
              ;; Update chain tip if this is the highest block
              (let ((current-tip (get-chain-tip database)))
                (when (or (not current-tip)
                          (> (get-block-height database block-hash)
                             (chain-state-tip-height current-tip)))
                  (set-chain-tip database (make-chain-state
                                           :tip-hash block-hash
                                           :tip-height (get-block-height database block-hash)
                                           :total-work 1
                                           :last-update (get-universal-time)))))
              
              (log:info "Processed block ~A at height ~A" 
                        (bitcoin-crawler.protocol:bytes-to-hex block-hash)
                        (get-block-height database block-hash)))))))))

(defun process-headers-message (database message)
  "Process a headers message"
  (let ((payload (bitcoin-crawler.protocol:message-payload message)))
    (let ((stream (flexi-streams:make-in-memory-input-stream payload)))
      (let ((count (bitcoin-crawler.protocol:deserialize-varint stream)))
        (log:info "Received ~A block headers" count)
        ;; In a real implementation, you would process the headers and request blocks
        ))))

(defun parse-transaction (stream)
  "Parse a transaction from stream"
  (let ((version (bitcoin-crawler.protocol:deserialize-uint32 stream))
        (input-count (bitcoin-crawler.protocol:deserialize-varint stream))
        (inputs '())
        (output-count (bitcoin-crawler.protocol:deserialize-varint stream))
        (outputs '()))
    
    ;; Parse inputs
    (loop for i from 0 below input-count
          do (let ((prev-hash (make-array 32 :element-type '(unsigned-byte 8))))
               (read-sequence prev-hash stream)
               (let ((prev-index (bitcoin-crawler.protocol:deserialize-uint32 stream))
                     (script-length (bitcoin-crawler.protocol:deserialize-varint stream))
                     (script (make-array script-length :element-type '(unsigned-byte 8))))
                 (read-sequence script stream)
                 (let ((sequence (bitcoin-crawler.protocol:deserialize-uint32 stream)))
                   (push (make-tx-input
                          :prev-hash prev-hash
                          :prev-index prev-index
                          :script-length script-length
                          :script script
                          :sequence sequence)
                         inputs)))))
    
    ;; Parse outputs
    (loop for i from 0 below output-count
          do (let ((value (bitcoin-crawler.protocol:deserialize-uint64 stream))
                   (script-length (bitcoin-crawler.protocol:deserialize-varint stream))
                   (script (make-array script-length :element-type '(unsigned-byte 8))))
               (read-sequence script stream)
               (push (make-tx-output
                      :value value
                      :script-length script-length
                      :script script)
                     outputs)))
    
    (let ((locktime (bitcoin-crawler.protocol:deserialize-uint32 stream)))
      (make-tx-data
       :version version
       :inputs (reverse inputs)
       :outputs (reverse outputs)
       :locktime locktime
       :size 0
       :weight 0
       :fee 0
       :confirmed-height nil
       :confirmed-hash nil)))))

(defun get-sync-progress (sync-manager)
  "Get synchronization progress"
  (let ((current (sync-current-height sync-manager))
        (target (sync-target-height sync-manager))
        (synced (sync-blocks-synced sync-manager))
        (start-time (sync-start-time sync-manager))
        (current-time (get-universal-time)))
    
    (list :current-height current
          :target-height target
          :blocks-synced synced
          :progress-percent (if (> target 0) (* 100 (/ current target)) 0)
          :sync-rate (if (> (- current-time start-time) 0)
                         (/ synced (- current-time start-time))
                         0)
          :running-p (sync-running-p sync-manager))))

(defun sync-to-height (database target-height)
  "Sync blockchain to a specific height"
  (let ((sync-manager (start-sync database :target-height target-height)))
    (loop while (and (sync-running-p sync-manager)
                     (< (sync-current-height sync-manager) target-height))
          do (sleep 1))
    (stop-sync sync-manager)
    sync-manager))

(defun sync-to-tip (database)
  "Sync blockchain to the current tip"
  (let ((best-peer (get-best-peer)))
    (if best-peer
        (let ((tip-height (peer-height best-peer)))
          (sync-to-height database tip-height))
        (error "No peers available for synchronization"))))

;; Utility functions
(defun get-chain-tip-height (database)
  "Get current chain tip height"
  (let ((chain-state (get-chain-tip database)))
    (if chain-state
        (chain-state-tip-height chain-state)
        0)))

(defun get-chain-tip-hash (database)
  "Get current chain tip hash"
  (let ((chain-state (get-chain-tip database)))
    (if chain-state
        (chain-state-tip-hash chain-state)
        nil)))

(defun is-block-valid (block-data)
  "Basic block validation"
  (and (block-data-header block-data)
       (block-data-transactions block-data)
       (> (length (block-data-transactions block-data)) 0)))

(defun calculate-block-work (block-header)
  "Calculate block work (simplified)"
  ;; In a real implementation, this would calculate the actual work
  1)

(defun update-chain-work (database block-hash)
  "Update chain work for a block"
  (let ((block-data (get-block database block-hash)))
    (when block-data
      (let ((work (calculate-block-work (block-data-header block-data))))
        (setf (block-data-chain-work block-data) work)
        (store-block database block-hash block-data)))))
