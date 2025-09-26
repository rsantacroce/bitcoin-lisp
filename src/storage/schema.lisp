(in-package :bitcoin-crawler.storage)

;; Bitcoin database schema definitions

;; Database key prefixes
(defconstant +BLOCK-PREFIX+ #(1))
(defconstant +TX-PREFIX+ #(2))
(defconstant +BLOCK-INDEX-PREFIX+ #(3))
(defconstant +CHAIN-TIP-PREFIX+ #(4))
(defconstant +UTXO-PREFIX+ #(5))
(defconstant +METADATA-PREFIX+ #(6))

;; Block storage schema
(defun make-block-key (block-hash)
  "Create database key for block"
  (concatenate 'vector +BLOCK-PREFIX+ block-hash))

(defun make-tx-key (tx-hash)
  "Create database key for transaction"
  (concatenate 'vector +TX-PREFIX+ tx-hash))

(defun make-block-index-key (height)
  "Create database key for block index (height -> block hash)"
  (concatenate 'vector +BLOCK-INDEX-PREFIX+ (bitcoin-crawler.protocol:serialize-uint32 height)))

(defun make-chain-tip-key ()
  "Create database key for chain tip"
  +CHAIN-TIP-PREFIX+)

(defun make-utxo-key (tx-hash output-index)
  "Create database key for UTXO"
  (concatenate 'vector +UTXO-PREFIX+ tx-hash (bitcoin-crawler.protocol:serialize-uint32 output-index)))

(defun make-metadata-key (key-name)
  "Create database key for metadata"
  (concatenate 'vector +METADATA-PREFIX+ (babel:string-to-octets key-name :encoding :utf-8)))

;; Block data structure
(defstruct (block-data (:conc-name block-data-))
  (header nil :type block-header)
  (transactions nil :type list)
  (size nil :type (unsigned-byte 32))
  (stripped-size nil :type (unsigned-byte 32))
  (weight nil :type (unsigned-byte 32))
  (height nil :type (unsigned-byte 32))
  (chain-work nil :type (unsigned-byte 256))
  (status nil :type keyword))

;; Transaction data structure
(defstruct (tx-data (:conc-name tx-data-))
  (version nil :type (unsigned-byte 32))
  (inputs nil :type list)
  (outputs nil :type list)
  (locktime nil :type (unsigned-byte 32))
  (size nil :type (unsigned-byte 32))
  (weight nil :type (unsigned-byte 32))
  (fee nil :type (unsigned-byte 64))
  (confirmed-height nil :type (or (unsigned-byte 32) null))
  (confirmed-hash nil :type (or (vector (unsigned-byte 8) 32) null)))

;; UTXO data structure
(defstruct (utxo-data (:conc-name utxo-data-))
  (tx-hash nil :type (vector (unsigned-byte 8) 32))
  (output-index nil :type (unsigned-byte 32))
  (value nil :type (unsigned-byte 64))
  (script nil :type (vector (unsigned-byte 8)))
  (height nil :type (unsigned-byte 32))
  (coinbase nil :type boolean))

;; Chain state structure
(defstruct (chain-state (:conc-name chain-state-))
  (tip-hash nil :type (vector (unsigned-byte 8) 32))
  (tip-height nil :type (unsigned-byte 32))
  (total-work nil :type (unsigned-byte 256))
  (last-update nil :type (unsigned-byte 32)))

;; Serialization functions for database storage
(defun serialize-block-data (block-data)
  "Serialize block data for storage"
  (let ((header-bytes (bitcoin-crawler.protocol:serialize-block-header (block-data-header block-data)))
        (tx-count (length (block-data-transactions block-data)))
        (tx-bytes (mapcar #'serialize-tx-data (block-data-transactions block-data))))
    (concatenate 'vector
                 (bitcoin-crawler.protocol:serialize-uint32 (block-data-version block-data))
                 header-bytes
                 (bitcoin-crawler.protocol:serialize-varint tx-count)
                 (apply #'concatenate 'vector tx-bytes)
                 (bitcoin-crawler.protocol:serialize-uint32 (block-data-size block-data))
                 (bitcoin-crawler.protocol:serialize-uint32 (block-data-stripped-size block-data))
                 (bitcoin-crawler.protocol:serialize-uint32 (block-data-weight block-data))
                 (bitcoin-crawler.protocol:serialize-uint32 (block-data-height block-data))
                 (bitcoin-crawler.protocol:serialize-uint256 (block-data-chain-work block-data))
                 (bitcoin-crawler.protocol:serialize-uint8 (if (eq (block-data-status block-data) :main) 1 0)))))

(defun deserialize-block-data (bytes)
  "Deserialize block data from storage"
  (let ((stream (flexi-streams:make-in-memory-input-stream bytes)))
    (make-block-data
     :version (bitcoin-crawler.protocol:deserialize-uint32 stream)
     :header (bitcoin-crawler.protocol:parse-block-header stream)
     :transactions (let ((tx-count (bitcoin-crawler.protocol:deserialize-varint stream)))
                     (loop for i from 0 below tx-count
                           collect (deserialize-tx-data stream)))
     :size (bitcoin-crawler.protocol:deserialize-uint32 stream)
     :stripped-size (bitcoin-crawler.protocol:deserialize-uint32 stream)
     :weight (bitcoin-crawler.protocol:deserialize-uint32 stream)
     :height (bitcoin-crawler.protocol:deserialize-uint32 stream)
     :chain-work (bitcoin-crawler.protocol:deserialize-uint256 stream)
     :status (if (= (bitcoin-crawler.protocol:deserialize-uint8 stream) 1) :main :orphan))))

(defun serialize-tx-data (tx-data)
  "Serialize transaction data for storage"
  (concatenate 'vector
               (bitcoin-crawler.protocol:serialize-uint32 (tx-data-version tx-data))
               (bitcoin-crawler.protocol:serialize-varint (length (tx-data-inputs tx-data)))
               (apply #'concatenate 'vector (mapcar #'serialize-tx-input (tx-data-inputs tx-data)))
               (bitcoin-crawler.protocol:serialize-varint (length (tx-data-outputs tx-data)))
               (apply #'concatenate 'vector (mapcar #'serialize-tx-output (tx-data-outputs tx-data)))
               (bitcoin-crawler.protocol:serialize-uint32 (tx-data-locktime tx-data))
               (bitcoin-crawler.protocol:serialize-uint32 (tx-data-size tx-data))
               (bitcoin-crawler.protocol:serialize-uint32 (tx-data-weight tx-data))
               (bitcoin-crawler.protocol:serialize-uint64 (tx-data-fee tx-data))
               (if (tx-data-confirmed-height tx-data)
                   (bitcoin-crawler.protocol:serialize-uint32 (tx-data-confirmed-height tx-data))
                   #(0 0 0 0))
               (if (tx-data-confirmed-hash tx-data)
                   (tx-data-confirmed-hash tx-data)
                   (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))))

(defun deserialize-tx-data (stream)
  "Deserialize transaction data from storage"
  (make-tx-data
   :version (bitcoin-crawler.protocol:deserialize-uint32 stream)
   :inputs (let ((input-count (bitcoin-crawler.protocol:deserialize-varint stream)))
             (loop for i from 0 below input-count
                   collect (deserialize-tx-input stream)))
   :outputs (let ((output-count (bitcoin-crawler.protocol:deserialize-varint stream)))
              (loop for i from 0 below output-count
                    collect (deserialize-tx-output stream)))
   :locktime (bitcoin-crawler.protocol:deserialize-uint32 stream)
   :size (bitcoin-crawler.protocol:deserialize-uint32 stream)
   :weight (bitcoin-crawler.protocol:deserialize-uint32 stream)
   :fee (bitcoin-crawler.protocol:deserialize-uint64 stream)
   :confirmed-height (let ((height (bitcoin-crawler.protocol:deserialize-uint32 stream)))
                       (if (> height 0) height nil))
   :confirmed-hash (let ((hash (make-array 32 :element-type '(unsigned-byte 8))))
                     (read-sequence hash stream)
                     (if (every #'zerop hash) nil hash))))

(defun serialize-tx-input (input)
  "Serialize transaction input for storage"
  (concatenate 'vector
               (tx-input-prev-hash input)
               (bitcoin-crawler.protocol:serialize-uint32 (tx-input-prev-index input))
               (bitcoin-crawler.protocol:serialize-varint (tx-input-script-length input))
               (tx-input-script input)
               (bitcoin-crawler.protocol:serialize-uint32 (tx-input-sequence input))))

(defun deserialize-tx-input (stream)
  "Deserialize transaction input from storage"
  (let ((prev-hash (make-array 32 :element-type '(unsigned-byte 8))))
    (read-sequence prev-hash stream)
    (make-tx-input
     :prev-hash prev-hash
     :prev-index (bitcoin-crawler.protocol:deserialize-uint32 stream)
     :script-length (bitcoin-crawler.protocol:deserialize-varint stream)
     :script (let ((script (make-array (bitcoin-crawler.protocol:deserialize-varint stream) :element-type '(unsigned-byte 8))))
               (read-sequence script stream)
               script)
     :sequence (bitcoin-crawler.protocol:deserialize-uint32 stream))))

(defun serialize-tx-output (output)
  "Serialize transaction output for storage"
  (concatenate 'vector
               (bitcoin-crawler.protocol:serialize-uint64 (tx-output-value output))
               (bitcoin-crawler.protocol:serialize-varint (tx-output-script-length output))
               (tx-output-script output)))

(defun deserialize-tx-output (stream)
  "Deserialize transaction output from storage"
  (make-tx-output
   :value (bitcoin-crawler.protocol:deserialize-uint64 stream)
   :script-length (bitcoin-crawler.protocol:deserialize-varint stream)
   :script (let ((script (make-array (bitcoin-crawler.protocol:deserialize-varint stream) :element-type '(unsigned-byte 8))))
             (read-sequence script stream)
             script)))

(defun serialize-utxo-data (utxo-data)
  "Serialize UTXO data for storage"
  (concatenate 'vector
               (utxo-data-tx-hash utxo-data)
               (bitcoin-crawler.protocol:serialize-uint32 (utxo-data-output-index utxo-data))
               (bitcoin-crawler.protocol:serialize-uint64 (utxo-data-value utxo-data))
               (bitcoin-crawler.protocol:serialize-varint (length (utxo-data-script utxo-data)))
               (utxo-data-script utxo-data)
               (bitcoin-crawler.protocol:serialize-uint32 (utxo-data-height utxo-data))
               (bitcoin-crawler.protocol:serialize-uint8 (if (utxo-data-coinbase utxo-data) 1 0))))

(defun deserialize-utxo-data (bytes)
  "Deserialize UTXO data from storage"
  (let ((stream (flexi-streams:make-in-memory-input-stream bytes)))
    (let ((tx-hash (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence tx-hash stream)
      (make-utxo-data
       :tx-hash tx-hash
       :output-index (bitcoin-crawler.protocol:deserialize-uint32 stream)
       :value (bitcoin-crawler.protocol:deserialize-uint64 stream)
       :script (let ((script-length (bitcoin-crawler.protocol:deserialize-varint stream)))
                 (let ((script (make-array script-length :element-type '(unsigned-byte 8))))
                   (read-sequence script stream)
                   script))
       :height (bitcoin-crawler.protocol:deserialize-uint32 stream)
       :coinbase (= (bitcoin-crawler.protocol:deserialize-uint8 stream) 1)))))

(defun serialize-chain-state (chain-state)
  "Serialize chain state for storage"
  (concatenate 'vector
               (chain-state-tip-hash chain-state)
               (bitcoin-crawler.protocol:serialize-uint32 (chain-state-tip-height chain-state))
               (bitcoin-crawler.protocol:serialize-uint256 (chain-state-total-work chain-state))
               (bitcoin-crawler.protocol:serialize-uint32 (chain-state-last-update chain-state))))

(defun deserialize-chain-state (bytes)
  "Deserialize chain state from storage"
  (let ((stream (flexi-streams:make-in-memory-input-stream bytes)))
    (let ((tip-hash (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence tip-hash stream)
      (make-chain-state
       :tip-hash tip-hash
       :tip-height (bitcoin-crawler.protocol:deserialize-uint32 stream)
       :total-work (bitcoin-crawler.protocol:deserialize-uint256 stream)
       :last-update (bitcoin-crawler.protocol:deserialize-uint32 stream)))))
