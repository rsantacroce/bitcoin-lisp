(in-package :bitcoin-crawler.protocol)

;; Bitcoin protocol message definitions and serialization

;; Message header structure
(defstruct (message-header (:conc-name header-))
  (magic nil :type (vector (unsigned-byte 8) 4))
  (command nil :type string)
  (length nil :type (unsigned-byte 32))
  (checksum nil :type (vector (unsigned-byte 8) 4)))

;; Base message class
(defclass bitcoin-message ()
  ((command :initarg :command :reader message-command :type string)
   (payload :initarg :payload :accessor message-payload :type (vector (unsigned-byte 8)))))

;; Version message
(defclass version-message (bitcoin-message)
  ((version :initarg :version :reader version-version :type (unsigned-byte 32))
   (services :initarg :services :reader version-services :type (unsigned-byte 64))
   (timestamp :initarg :timestamp :reader version-timestamp :type (unsigned-byte 64))
   (addr-recv :initarg :addr-recv :reader version-addr-recv :type (vector (unsigned-byte 8) 26))
   (addr-from :initarg :addr-from :reader version-addr-from :type (vector (unsigned-byte 8) 26))
   (nonce :initarg :nonce :reader version-nonce :type (unsigned-byte 64))
   (user-agent :initarg :user-agent :reader version-user-agent :type string)
   (start-height :initarg :start-height :reader version-start-height :type (unsigned-byte 32))
   (relay :initarg :relay :reader version-relay :type boolean)))

;; Verack message
(defclass verack-message (bitcoin-message)
  ())

;; Ping message
(defclass ping-message (bitcoin-message)
  ((nonce :initarg :nonce :reader ping-nonce :type (unsigned-byte 64))))

;; Pong message
(defclass pong-message (bitcoin-message)
  ((nonce :initarg :nonce :reader pong-nonce :type (unsigned-byte 64))))

;; Getblocks message
(defclass getblocks-message (bitcoin-message)
  ((version :initarg :version :reader getblocks-version :type (unsigned-byte 32))
   (hash-count :initarg :hash-count :reader getblocks-hash-count :type (unsigned-byte 32))
   (block-locator-hashes :initarg :block-locator-hashes :reader getblocks-hashes :type list)
   (hash-stop :initarg :hash-stop :reader getblocks-hash-stop :type (vector (unsigned-byte 8) 32))))

;; Getdata message
(defclass getdata-message (bitcoin-message)
  ((count :initarg :count :reader getdata-count :type (unsigned-byte 32))
   (inventory :initarg :inventory :reader getdata-inventory :type list)))

;; Inv message
(defclass inv-message (bitcoin-message)
  ((count :initarg :count :reader inv-count :type (unsigned-byte 32))
   (inventory :initarg :inventory :reader inv-inventory :type list)))

;; Block message
(defclass block-message (bitcoin-message)
  ((header :initarg :header :reader block-header :type block-header)
   (tx-count :initarg :tx-count :reader block-tx-count :type (unsigned-byte 32))
   (transactions :initarg :transactions :reader block-transactions :type list)))

;; Transaction message
(defclass tx-message (bitcoin-message)
  ((version :initarg :version :reader tx-version :type (unsigned-byte 32))
   (input-count :initarg :input-count :reader tx-input-count :type (unsigned-byte 32))
   (inputs :initarg :inputs :reader tx-inputs :type list)
   (output-count :initarg :output-count :reader tx-output-count :type (unsigned-byte 32))
   (outputs :initarg :outputs :reader tx-outputs :type list)
   (locktime :initarg :locktime :reader tx-locktime :type (unsigned-byte 32))))

;; Headers message
(defclass headers-message (bitcoin-message)
  ((count :initarg :count :reader headers-count :type (unsigned-byte 32))
   (headers :initarg :headers :reader headers-headers :type list)))

;; Block header structure
(defstruct (block-header (:conc-name block-))
  (version nil :type (unsigned-byte 32))
  (prev-block-hash nil :type (vector (unsigned-byte 8) 32))
  (merkle-root nil :type (vector (unsigned-byte 8) 32))
  (timestamp nil :type (unsigned-byte 32))
  (bits nil :type (unsigned-byte 32))
  (nonce nil :type (unsigned-byte 32)))

;; Inventory item structure
(defstruct (inventory-item (:conc-name inv-))
  (type nil :type (unsigned-byte 32))
  (hash nil :type (vector (unsigned-byte 8) 32)))

;; Transaction input structure
(defstruct (tx-input (:conc-name tx-input-))
  (prev-hash nil :type (vector (unsigned-byte 8) 32))
  (prev-index nil :type (unsigned-byte 32))
  (script-length nil :type (unsigned-byte 32))
  (script nil :type (vector (unsigned-byte 8)))
  (sequence nil :type (unsigned-byte 32)))

;; Transaction output structure
(defstruct (tx-output (:conc-name tx-output-))
  (value nil :type (unsigned-byte 64))
  (script-length nil :type (unsigned-byte 32))
  (script nil :type (vector (unsigned-byte 8))))

;; Message creation functions
(defun make-version (services timestamp addr-recv addr-from nonce user-agent start-height)
  "Create a version message"
  (make-instance 'version-message
                 :command "version"
                 :version *bitcoin-protocol-version*
                 :services services
                 :timestamp timestamp
                 :addr-recv addr-recv
                 :addr-from addr-from
                 :nonce nonce
                 :user-agent user-agent
                 :start-height start-height
                 :relay t
                 :payload (serialize-version services timestamp addr-recv addr-from nonce user-agent start-height)))

(defun make-verack ()
  "Create a verack message"
  (make-instance 'verack-message
                 :command "verack"
                 :payload #()))

(defun make-ping (nonce)
  "Create a ping message"
  (make-instance 'ping-message
                 :command "ping"
                 :nonce nonce
                 :payload (serialize-uint64 nonce)))

(defun make-pong (nonce)
  "Create a pong message"
  (make-instance 'pong-message
                 :command "pong"
                 :nonce nonce
                 :payload (serialize-uint64 nonce)))

(defun make-getblocks (version block-locator-hashes hash-stop)
  "Create a getblocks message"
  (make-instance 'getblocks-message
                 :command "getblocks"
                 :version version
                 :hash-count (length block-locator-hashes)
                 :block-locator-hashes block-locator-hashes
                 :hash-stop hash-stop
                 :payload (serialize-getblocks version block-locator-hashes hash-stop)))

(defun make-getdata (inventory)
  "Create a getdata message"
  (make-instance 'getdata-message
                 :command "getdata"
                 :count (length inventory)
                 :inventory inventory
                 :payload (serialize-getdata inventory)))

(defun make-inv (inventory)
  "Create an inv message"
  (make-instance 'inv-message
                 :command "inv"
                 :count (length inventory)
                 :inventory inventory
                 :payload (serialize-inv inventory)))

;; Serialization functions
(defun serialize-version (services timestamp addr-recv addr-from nonce user-agent start-height)
  "Serialize version message payload"
  (concatenate 'vector
               (serialize-uint32 *bitcoin-protocol-version*)
               (serialize-uint64 services)
               (serialize-uint64 timestamp)
               addr-recv
               addr-from
               (serialize-uint64 nonce)
               (serialize-varstr user-agent)
               (serialize-uint32 start-height)
               (serialize-uint8 (if t 1 0))))

(defun serialize-getblocks (version block-locator-hashes hash-stop)
  "Serialize getblocks message payload"
  (let ((result (concatenate 'vector
                             (serialize-uint32 version)
                             (serialize-varint (length block-locator-hashes)))))
    (dolist (hash block-locator-hashes)
      (setf result (concatenate 'vector result hash)))
    (concatenate 'vector result hash-stop)))

(defun serialize-getdata (inventory)
  "Serialize getdata message payload"
  (let ((result (concatenate 'vector (serialize-varint (length inventory)))))
    (dolist (item inventory)
      (setf result (concatenate 'vector
                                result
                                (serialize-uint32 (inv-type item))
                                (inv-hash item))))
    result))

(defun serialize-inv (inventory)
  "Serialize inv message payload"
  (let ((result (concatenate 'vector (serialize-varint (length inventory)))))
    (dolist (item inventory)
      (setf result (concatenate 'vector
                                result
                                (serialize-uint32 (inv-type item))
                                (inv-hash item))))
    result))

(defun serialize-block-header (header)
  "Serialize block header"
  (concatenate 'vector
               (serialize-uint32 (block-version header))
               (block-prev-block-hash header)
               (block-merkle-root header)
               (serialize-uint32 (block-timestamp header))
               (serialize-uint32 (block-bits header))
               (serialize-uint32 (block-nonce header))))

(defun serialize-tx-input (input)
  "Serialize transaction input"
  (concatenate 'vector
               (tx-input-prev-hash input)
               (serialize-uint32 (tx-input-prev-index input))
               (serialize-varint (tx-input-script-length input))
               (tx-input-script input)
               (serialize-uint32 (tx-input-sequence input))))

(defun serialize-tx-output (output)
  "Serialize transaction output"
  (concatenate 'vector
               (serialize-uint64 (tx-output-value output))
               (serialize-varint (tx-output-script-length output))
               (tx-output-script output)))

;; Message parsing functions
(defun parse-message-header (stream)
  "Parse Bitcoin message header from stream"
  (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (let ((command-bytes (make-array 12 :element-type '(unsigned-byte 8))))
      (read-sequence command-bytes stream)
      (let ((command (babel:octets-to-string command-bytes :encoding :ascii)))
        (setf command (string-trim " " command))
        (let ((length (deserialize-uint32 stream))
              (checksum (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence checksum stream)
          (make-message-header :magic magic
                               :command command
                               :length length
                               :checksum checksum))))))

(defun parse-version-message (payload)
  "Parse version message from payload"
  (let ((stream (flexi-streams:make-in-memory-input-stream payload)))
    (make-instance 'version-message
                   :command "version"
                   :version (deserialize-uint32 stream)
                   :services (deserialize-uint64 stream)
                   :timestamp (deserialize-uint64 stream)
                   :addr-recv (let ((addr (make-array 26 :element-type '(unsigned-byte 8))))
                                (read-sequence addr stream)
                                addr)
                   :addr-from (let ((addr (make-array 26 :element-type '(unsigned-byte 8))))
                                (read-sequence addr stream)
                                addr)
                   :nonce (deserialize-uint64 stream)
                   :user-agent (deserialize-varstr stream)
                   :start-height (deserialize-uint32 stream)
                   :relay (= (deserialize-uint8 stream) 1)
                   :payload payload)))

(defun parse-block-header (stream)
  "Parse block header from stream"
  (make-block-header
   :version (deserialize-uint32 stream)
   :prev-block-hash (let ((hash (make-array 32 :element-type '(unsigned-byte 8))))
                      (read-sequence hash stream)
                      hash)
   :merkle-root (let ((hash (make-array 32 :element-type '(unsigned-byte 8))))
                  (read-sequence hash stream)
                  hash)
   :timestamp (deserialize-uint32 stream)
   :bits (deserialize-uint32 stream)
   :nonce (deserialize-uint32 stream)))

;; Message serialization for network transmission
(defun serialize-message (message)
  "Serialize a complete Bitcoin message for network transmission"
  (let* ((command (message-command message))
         (payload (message-payload message))
         (command-bytes (babel:string-to-octets command :encoding :ascii))
         (padded-command (concatenate 'vector command-bytes (make-array (- 12 (length command-bytes)) :initial-element 0)))
         (length (length payload))
         (checksum (subseq (double-sha256 payload) 0 4)))
    (concatenate 'vector
                 *bitcoin-magic-bytes*
                 padded-command
                 (serialize-uint32 length)
                 checksum
                 payload)))

(defun parse-message (stream)
  "Parse a complete Bitcoin message from stream"
  (let ((header (parse-message-header stream)))
    (when (not (equalp (header-magic header) *bitcoin-magic-bytes*))
      (error "Invalid magic bytes: ~A" (header-magic header)))
    (let ((payload (make-array (header-length header) :element-type '(unsigned-byte 8))))
      (read-sequence payload stream)
      (let ((expected-checksum (subseq (double-sha256 payload) 0 4)))
        (when (not (equalp expected-checksum (header-checksum header)))
          (error "Checksum mismatch")))
      (case (intern (string-upcase (header-command header)) :keyword)
        (:version (parse-version-message payload))
        (:verack (make-verack))
        (:ping (make-ping (deserialize-uint64 (flexi-streams:make-in-memory-input-stream payload))))
        (:pong (make-pong (deserialize-uint64 (flexi-streams:make-in-memory-input-stream payload))))
        (t (make-instance 'bitcoin-message
                          :command (header-command header)
                          :payload payload))))))
