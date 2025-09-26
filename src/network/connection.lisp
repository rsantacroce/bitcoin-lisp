(in-package :bitcoin-crawler.network)

;; Import required functions
(import '(bitcoin-crawler.protocol:serialize-message
          bitcoin-crawler.protocol:parse-message-header
          bitcoin-crawler.protocol:parse-message
          bitcoin-crawler.protocol:make-version
          bitcoin-crawler.protocol:make-verack
          bitcoin-crawler.protocol:make-ping
          bitcoin-crawler.protocol:make-pong
          bitcoin-crawler.protocol:make-getdata
          bitcoin-crawler.protocol:make-getblocks
          bitcoin-crawler.protocol:serialize-uint32
          bitcoin-crawler.protocol:deserialize-uint32
          bitcoin-crawler.protocol:deserialize-uint64
          bitcoin-crawler.protocol:deserialize-uint8
          bitcoin-crawler.protocol:deserialize-varstr
          bitcoin-crawler.protocol:serialize-uint64
          bitcoin-crawler.protocol:serialize-uint8
          bitcoin-crawler.protocol:serialize-varstr
          bitcoin-crawler.protocol:serialize-ipv6
          bitcoin-crawler.protocol:deserialize-ipv6
          bitcoin-crawler.protocol:serialize-ipv4
          bitcoin-crawler.protocol:deserialize-ipv4
          bitcoin-crawler.protocol:serialize-getblocks
          bitcoin-crawler.protocol:serialize-getdata
          bitcoin-crawler.protocol:make-inventory-item
          bitcoin-crawler.protocol:make-getblocks
          bitcoin-crawler.protocol:make-getdata
          bitcoin-crawler.protocol:ping-nonce
          bitcoin-crawler.protocol:pong-nonce
          bitcoin-crawler.protocol:version-version
          bitcoin-crawler.protocol:message-command
          bitcoin-crawler.protocol:message-payload
          bitcoin-crawler.protocol:serialize-block-header
          bitcoin-crawler.protocol:parse-block-header
          bitcoin-crawler.protocol:bitcoin-hash
          bitcoin-crawler.protocol:double-sha256
          bitcoin-crawler.protocol:header-length
          bitcoin-crawler.protocol:*bitcoin-protocol-version*
          bitcoin-crawler.protocol:*bitcoin-magic-bytes*))

;; Bitcoin network connection management

(defclass bitcoin-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (address :initarg :address :accessor connection-address)
   (port :initarg :port :accessor connection-port)
   (connected-p :initarg :connected-p :accessor connection-connected-p :initform nil)
   (last-seen :initarg :last-seen :accessor connection-last-seen :initform (get-universal-time))
   (services :initarg :services :accessor connection-services :initform 0)
   (version :initarg :version :accessor connection-version :initform 0)
   (height :initarg :height :accessor connection-height :initform 0)
   (user-agent :initarg :user-agent :accessor connection-user-agent :initform "")))

(defun connect-to-peer (address port &key (timeout 30))
  "Connect to a Bitcoin peer"
  (let ((socket nil))
    (handler-case
        (progn
          (setf socket (usocket:socket-connect address port :timeout timeout))
          (make-instance 'bitcoin-connection
                         :socket socket
                         :address address
                         :port port
                         :connected-p t))
      (error (e)
        (when socket
          (usocket:socket-close socket))
        (error "Failed to connect to ~A:~A: ~A" address port e)))))

(defun disconnect-peer (connection)
  "Disconnect from a Bitcoin peer"
  (when (connection-socket connection)
    (handler-case
        (usocket:socket-close (connection-socket connection))
      (error (e)
        (log:warn "Error closing socket: ~A" e)))
    (setf (connection-socket connection) nil
          (connection-connected-p connection) nil)))

(defun connection-alive-p (connection)
  "Check if connection is still alive"
  (and (connection-connected-p connection)
       (connection-socket connection)
       (usocket:socket-open-p (connection-socket connection))))

(defun send-message (connection message)
  "Send a Bitcoin message to peer"
  (unless (connection-alive-p connection)
    (error "Connection is not alive"))
  (let ((data (serialize-message message)))
    (handler-case
        (progn
          (usocket:socket-send (connection-socket connection) data nil)
          (setf (connection-last-seen connection) (get-universal-time))
          t)
      (error (e)
        (log:error "Failed to send message: ~A" e)
        (setf (connection-connected-p connection) nil)
        nil))))

(defun receive-message (connection &key (timeout 60))
  "Receive a Bitcoin message from peer"
  (unless (connection-alive-p connection)
    (error "Connection is not alive"))
  (handler-case
      (let* ((header-bytes (make-array 24 :element-type '(unsigned-byte 8)))
             (stream (usocket:socket-stream (connection-socket connection))))
        ;; Read message header (24 bytes)
        (read-sequence header-bytes stream)
        (let ((header-stream (flexi-streams:make-in-memory-input-stream header-bytes)))
          (let ((header (parse-message-header header-stream)))
            ;; Read message payload
            (let ((payload (make-array (header-length header) :element-type '(unsigned-byte 8))))
              (read-sequence payload stream)
              (let ((payload-stream (flexi-streams:make-in-memory-input-stream payload)))
                (parse-message payload-stream))))))
    (error (e)
      (log:error "Failed to receive message: ~A" e)
      (setf (connection-connected-p connection) nil)
      nil)))

(defun send-version-handshake (connection &key (services 1) (user-agent "Bitcoin-Crawler/0.1.0"))
  "Send version handshake to peer"
  (let* ((timestamp (get-universal-time))
         (nonce (random (expt 2 64)))
         (addr-recv (make-array 26 :element-type '(unsigned-byte 8) :initial-element 0))
         (addr-from (make-array 26 :element-type '(unsigned-byte 8) :initial-element 0))
         (version-msg (make-version services timestamp addr-recv addr-from nonce user-agent 0)))
    (send-message connection version-msg)))

(defun wait-for-verack (connection &key (timeout 30))
  "Wait for verack message from peer"
  (let ((start-time (get-universal-time)))
    (loop
      (when (> (- (get-universal-time) start-time) timeout)
        (error "Timeout waiting for verack"))
      (let ((message (receive-message connection :timeout 1)))
        (when message
          (case (intern (string-upcase (message-command message)) :keyword)
            (:verack (return t))
            (:version (progn
                        (log:info "Received version message from ~A:~A" 
                                  (connection-address connection) (connection-port connection))
                        (send-message connection (make-verack))))
            (t (log:debug "Received message: ~A" (message-command message)))))))))

(defun perform-handshake (connection)
  "Perform complete Bitcoin handshake"
  (log:info "Starting handshake with ~A:~A" (connection-address connection) (connection-port connection))
  (send-version-handshake connection)
  (wait-for-verack connection)
  (log:info "Handshake completed with ~A:~A" (connection-address connection) (connection-port connection))
  t)

(defun ping-peer (connection)
  "Send ping to peer"
  (let ((nonce (random (expt 2 64))))
    (send-message connection (make-ping nonce))
    nonce))

(defun pong-peer (connection nonce)
  "Send pong to peer"
  (send-message connection (make-pong nonce)))

(defun request-blocks (connection block-hashes)
  "Request blocks from peer"
  (let ((inventory (mapcar (lambda (hash)
                             (make-inventory-item :type 2 :hash hash))
                           block-hashes)))
    (send-message connection (make-getdata inventory))))

(defun request-headers (connection block-locator-hashes hash-stop)
  "Request block headers from peer"
  (send-message connection (make-getblocks 70015 block-locator-hashes hash-stop)))

;; Connection pool management
(defvar *connection-pool* (make-hash-table :test 'equal))

(defun add-connection (address port connection)
  "Add connection to pool"
  (setf (gethash (format nil "~A:~A" address port) *connection-pool*) connection))

(defun remove-connection (address port)
  "Remove connection from pool"
  (let ((key (format nil "~A:~A" address port)))
    (when (gethash key *connection-pool*)
      (disconnect-peer (gethash key *connection-pool*))
      (remhash key *connection-pool*))))

(defun get-connection (address port)
  "Get connection from pool"
  (gethash (format nil "~A:~A" address port) *connection-pool*))

(defun list-connections ()
  "List all active connections"
  (let ((connections '()))
    (maphash (lambda (key connection)
               (declare (ignore key))
               (when (connection-alive-p connection)
                 (push connection connections)))
             *connection-pool*)
    connections))

(defun cleanup-dead-connections ()
  "Remove dead connections from pool"
  (let ((to-remove '()))
    (maphash (lambda (key connection)
               (unless (connection-alive-p connection)
                 (push key to-remove)))
             *connection-pool*)
    (dolist (key to-remove)
      (remhash key *connection-pool*))))

(defun get-connection-count ()
  "Get number of active connections"
  (length (list-connections)))
