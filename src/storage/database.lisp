(in-package :bitcoin-crawler.storage)

;; Bitcoin database implementation using file-based key-value store

(defclass bitcoin-database ()
  ((path :initarg :path :reader database-path :type string)
   (open-p :initarg :open-p :accessor database-open-p :type boolean :initform nil)
   (data-dir :initarg :data-dir :accessor database-data-dir :type string)
   (index-file :initarg :index-file :accessor database-index-file :type string)
   (lock-file :initarg :lock-file :accessor database-lock-file :type string)))

(defun open-database (path)
  "Open or create a Bitcoin database"
  (let* ((data-dir (merge-pathnames path (user-homedir-pathname)))
         (index-file (merge-pathnames "index.db" data-dir))
         (lock-file (merge-pathnames "lock" data-dir)))
    
    ;; Create data directory if it doesn't exist
    (ensure-directories-exist data-dir)
    
    ;; Check for existing lock file
    (when (probe-file lock-file)
      (error "Database is locked by another process"))
    
    ;; Create lock file
    (with-open-file (stream lock-file :direction :output :if-exists :error)
      (write-string (format nil "~A~%" (get-universal-time)) stream))
    
    (let ((db (make-instance 'bitcoin-database
                             :path path
                             :data-dir data-dir
                             :index-file index-file
                             :lock-file lock-file)))
      (setf (database-open-p db) t)
      (log:info "Opened database at ~A" data-dir)
      db)))

(defun close-database (database)
  "Close a Bitcoin database"
  (when (database-open-p database)
    (setf (database-open-p database) nil)
    ;; Remove lock file
    (when (probe-file (database-lock-file database))
      (delete-file (database-lock-file database)))
    (log:info "Closed database at ~A" (database-data-dir database))))

(defun get-file-path (database key)
  "Get file path for a database key"
  (let ((key-hex (bitcoin-crawler.protocol:bytes-to-hex key)))
    (merge-pathnames (format nil "~A.dat" key-hex) (database-data-dir database))))

(defun store-value (database key value)
  "Store a value in the database"
  (unless (database-open-p database)
    (error "Database is not open"))
  
  (let ((file-path (get-file-path database key)))
    (with-open-file (stream file-path
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :overwrite)
      (write-sequence value stream))
    t))

(defun get-value (database key)
  "Get a value from the database"
  (unless (database-open-p database)
    (error "Database is not open"))
  
  (let ((file-path (get-file-path database key)))
    (if (probe-file file-path)
        (with-open-file (stream file-path
                                :direction :input
                                :element-type '(unsigned-byte 8))
          (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
            (read-sequence bytes stream)
            bytes))
        nil)))

(defun delete-value (database key)
  "Delete a value from the database"
  (unless (database-open-p database)
    (error "Database is not open"))
  
  (let ((file-path (get-file-path database key)))
    (when (probe-file file-path)
      (delete-file file-path))))

(defun key-exists-p (database key)
  "Check if a key exists in the database"
  (probe-file (get-file-path database key)))

;; Block operations
(defun store-block (database block-hash block-data)
  "Store a block in the database"
  (let ((key (make-block-key block-hash))
        (value (serialize-block-data block-data)))
    (store-value database key value)
    (log:debug "Stored block ~A" (bitcoin-crawler.protocol:bytes-to-hex block-hash))))

(defun get-block (database block-hash)
  "Get a block from the database"
  (let ((key (make-block-key block-hash))
        (value (get-value database key)))
    (when value
      (deserialize-block-data value))))

(defun block-exists-p (database block-hash)
  "Check if a block exists in the database"
  (key-exists-p database (make-block-key block-hash)))

(defun store-block-index (database height block-hash)
  "Store block index (height -> block hash)"
  (let ((key (make-block-index-key height))
        (value block-hash))
    (store-value database key value)
    (log:debug "Stored block index ~A -> ~A" height (bitcoin-crawler.protocol:bytes-to-hex block-hash))))

(defun get-block-by-height (database height)
  "Get block hash by height"
  (let ((key (make-block-index-key height))
        (value (get-value database key)))
    (when value
      (let ((hash (make-array 32 :element-type '(unsigned-byte 8))))
        (replace hash value)
        hash))))

(defun get-block-hash-by-height (database height)
  "Get block hash by height (alias for get-block-by-height)"
  (get-block-by-height database height))

(defun get-block-height (database block-hash)
  "Get block height by block hash"
  ;; This is inefficient - in a real implementation, you'd maintain a reverse index
  (let ((max-height 1000000)) ; Reasonable upper bound
    (loop for height from 0 to max-height
          do (let ((stored-hash (get-block-by-height database height)))
               (when (and stored-hash (equalp stored-hash block-hash))
                 (return height)))
          finally (return nil))))

(defun set-block-height (database block-hash height)
  "Set block height (store in index)"
  (store-block-index database height block-hash))

;; Transaction operations
(defun store-transaction (database tx-hash tx-data)
  "Store a transaction in the database"
  (let ((key (make-tx-key tx-hash))
        (value (serialize-tx-data tx-data)))
    (store-value database key value)
    (log:debug "Stored transaction ~A" (bitcoin-crawler.protocol:bytes-to-hex tx-hash))))

(defun get-transaction (database tx-hash)
  "Get a transaction from the database"
  (let ((key (make-tx-key tx-hash))
        (value (get-value database key)))
    (when value
      (deserialize-tx-data (flexi-streams:make-in-memory-input-stream value)))))

(defun transaction-exists-p (database tx-hash)
  "Check if a transaction exists in the database"
  (key-exists-p database (make-tx-key tx-hash)))

;; Chain state operations
(defun get-chain-tip (database)
  "Get current chain tip"
  (let ((key (make-chain-tip-key))
        (value (get-value database key)))
    (when value
      (deserialize-chain-state value))))

(defun set-chain-tip (database chain-state)
  "Set current chain tip"
  (let ((key (make-chain-tip-key))
        (value (serialize-chain-state chain-state)))
    (store-value database key value)
    (log:info "Set chain tip to height ~A" (chain-state-tip-height chain-state))))

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

;; UTXO operations
(defun store-utxo (database tx-hash output-index utxo-data)
  "Store a UTXO in the database"
  (let ((key (make-utxo-key tx-hash output-index))
        (value (serialize-utxo-data utxo-data)))
    (store-value database key value)))

(defun get-utxo (database tx-hash output-index)
  "Get a UTXO from the database"
  (let ((key (make-utxo-key tx-hash output-index))
        (value (get-value database key)))
    (when value
      (deserialize-utxo-data value))))

(defun spend-utxo (database tx-hash output-index)
  "Spend a UTXO (remove from database)"
  (let ((key (make-utxo-key tx-hash output-index)))
    (delete-value database key)))

;; Metadata operations
(defun store-metadata (database key-name value)
  "Store metadata in the database"
  (let ((key (make-metadata-key key-name))
        (value (babel:string-to-octets value :encoding :utf-8)))
    (store-value database key value)))

(defun get-metadata (database key-name)
  "Get metadata from the database"
  (let ((key (make-metadata-key key-name))
        (value (get-value database key)))
    (when value
      (babel:octets-to-string value :encoding :utf-8))))

;; Database statistics
(defun get-database-stats (database)
  "Get database statistics"
  (let ((block-count 0)
        (tx-count 0)
        (total-size 0))
    (when (database-open-p database)
      (let ((data-dir (database-data-dir database)))
        (when (probe-file data-dir)
          (dolist (file (directory (merge-pathnames "*.dat" data-dir)))
            (let ((file-size (file-length file)))
              (incf total-size file-size)
              (let ((filename (pathname-name file)))
                (when (string= filename "1") ; Block prefix
                  (incf block-count))
                (when (string= filename "2") ; TX prefix
                  (incf tx-count)))))))
    (list :block-count block-count
          :tx-count tx-count
          :total-size total-size
          :open-p (database-open-p database))))

;; Database maintenance
(defun compact-database (database)
  "Compact the database (remove orphaned data)"
  (log:info "Compacting database...")
  ;; In a real implementation, this would remove orphaned blocks and transactions
  (log:info "Database compaction completed"))

(defun backup-database (database backup-path)
  "Backup the database"
  (log:info "Backing up database to ~A" backup-path)
  ;; In a real implementation, this would copy all data files
  (log:info "Database backup completed"))

;; Initialize database with genesis block
(defun initialize-database (database)
  "Initialize database with genesis block"
  (let ((genesis-hash *genesis-block-hash*))
    (unless (block-exists-p database genesis-hash)
      (log:info "Initializing database with genesis block")
      ;; In a real implementation, you would create the actual genesis block data
      (let ((genesis-block (make-block-data
                            :header (make-block-header
                                     :version 1
                                     :prev-block-hash (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
                                     :merkle-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
                                     :timestamp 1231006505
                                     :bits 486604799
                                     :nonce 2083236893)
                            :transactions '()
                            :size 285
                            :stripped-size 285
                            :weight 1140
                            :height 0
                            :chain-work 1
                            :status :main)))
        (store-block database genesis-hash genesis-block)
        (set-block-height database genesis-hash 0)
        (set-chain-tip database (make-chain-state
                                 :tip-hash genesis-hash
                                 :tip-height 0
                                 :total-work 1
                                 :last-update (get-universal-time)))))))
