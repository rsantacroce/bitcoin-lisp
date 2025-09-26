(in-package :bitcoin-crawler)

;; Bitcoin network configuration
(defparameter *bitcoin-magic-bytes* #(#x0B #x11 #x09 #x07) "Bitcoin mainnet magic bytes")
(defparameter *bitcoin-protocol-version* 70015 "Bitcoin protocol version")
(defparameter *bitcoin-default-port* 8333 "Default Bitcoin P2P port")

;; Seed nodes for peer discovery
(defparameter *seed-nodes*
  '("seed.bitcoin.sipa.be:8333"
    "dnsseed.bluematt.me:8333"
    "dnsseed.bitcoin.dashjr.org:8333"
    "seed.bitcoinstats.com:8333"
    "seed.bitcoin.jonasschnelli.ch:8333")
  "List of Bitcoin seed nodes")

;; Network configuration
(defparameter *max-peers* 8 "Maximum number of peer connections")
(defparameter *connection-timeout* 30 "Connection timeout in seconds")
(defparameter *message-timeout* 60 "Message timeout in seconds")
(defparameter *ping-interval* 120 "Ping interval in seconds")
(defparameter *reconnect-delay* 300 "Reconnect delay in seconds")

;; Sync configuration
(defparameter *sync-batch-size* 500 "Number of blocks to request in each batch")
(defparameter *max-orphan-blocks* 100 "Maximum number of orphan blocks to keep")
(defparameter *sync-timeout* 300 "Sync operation timeout in seconds")

;; Database configuration
(defparameter *database-path* "./bitcoin-data" "Path to store blockchain data")
(defparameter *database-cache-size* 1000000 "Database cache size in bytes")

;; Logging configuration
(defparameter *log-level* :info "Logging level (:debug, :info, :warn, :error)")
(defparameter *log-file* "./bitcoin-crawler.log" "Log file path")

;; Genesis block hash (Bitcoin mainnet)
(defparameter *genesis-block-hash*
  #(#x6F #xE2 #x8C #x0A #xB6 #xF1 #xB3 #x72 #xC1 #xA6 #xA2 #x46 #xAE #x63 #xF7 #x4F
    #x93 #x4E #x4F #x13 #x17 #x93 #x8A #x82 #x70 #x3B #x0A #x10 #x9C #x51 #x2C #x22 #x56)
  "Bitcoin genesis block hash")

;; Configuration management
(defvar *config* (make-hash-table :test 'equal))

(defun load-config (&optional config-file)
  "Load configuration from file or use defaults"
  (setf (gethash "database-path" *config*) *database-path*
        (gethash "max-peers" *config*) *max-peers*
        (gethash "connection-timeout" *config*) *connection-timeout*
        (gethash "message-timeout" *config*) *message-timeout*
        (gethash "sync-batch-size" *config*) *sync-batch-size*
        (gethash "log-level" *config*) *log-level*
        (gethash "log-file" *config*) *log-file*)
  (when config-file
    (with-open-file (stream config-file :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((pos (position #\= line)))
                 (when pos
                   (let ((key (string-trim " " (subseq line 0 pos)))
                         (value (string-trim " " (subseq line (1+ pos)))))
                     (setf (gethash key *config*) value))))))))

(defun get-config (key &optional default)
  "Get configuration value by key"
  (gethash key *config* default))

(defun set-config (key value)
  "Set configuration value"
  (setf (gethash key *config*) value))

;; Initialize default configuration
(load-config)
