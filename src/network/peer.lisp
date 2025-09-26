(in-package :bitcoin-crawler.network)

;; Bitcoin peer management

(defclass bitcoin-peer ()
  ((address :initarg :address :reader peer-address :type string)
   (port :initarg :port :reader peer-port :type (unsigned-byte 16))
   (connection :initarg :connection :accessor peer-connection :type (or bitcoin-connection null))
   (last-seen :initarg :last-seen :accessor peer-last-seen :type (unsigned-byte 32) :initform 0)
   (services :initarg :services :accessor peer-services :type (unsigned-byte 64) :initform 0)
   (version :initarg :version :accessor peer-version :type (unsigned-byte 32) :initform 0)
   (height :initarg :height :accessor peer-height :type (unsigned-byte 32) :initform 0)
   (user-agent :initarg :user-agent :accessor peer-user-agent :type string :initform "")
   (connected-p :initarg :connected-p :accessor peer-connected-p :type boolean :initform nil)
   (last-ping :initarg :last-ping :accessor peer-last-ping :type (unsigned-byte 64) :initform 0)
   (last-pong :initarg :last-pong :accessor peer-last-pong :type (unsigned-byte 64) :initform 0)))

(defvar *peer-database* (make-hash-table :test 'equal))

(defun add-peer (address port &key (services 0) (version 0) (height 0) (user-agent ""))
  "Add a peer to the database"
  (let ((key (format nil "~A:~A" address port)))
    (setf (gethash key *peer-database*)
          (make-instance 'bitcoin-peer
                         :address address
                         :port port
                         :services services
                         :version version
                         :height height
                         :user-agent user-agent))))

(defun remove-peer (address port)
  "Remove a peer from the database"
  (let ((key (format nil "~A:~A" address port)))
    (let ((peer (gethash key *peer-database*)))
      (when peer
        (when (peer-connection peer)
          (disconnect-peer (peer-connection peer)))
        (remhash key *peer-database*)))))

(defun get-peer (address port)
  "Get a peer from the database"
  (gethash (format nil "~A:~A" address port) *peer-database*))

(defun list-peers ()
  "List all peers in the database"
  (let ((peers '()))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (push peer peers))
             *peer-database*)
    peers))

(defun connect-to-peer (address port &key (timeout 30))
  "Connect to a peer and establish Bitcoin protocol connection"
  (let ((peer (get-peer address port)))
    (unless peer
      (setf peer (add-peer address port)))
    
    (handler-case
        (let ((connection (connect-to-peer address port :timeout timeout)))
          (setf (peer-connection peer) connection
                (peer-connected-p peer) t
                (peer-last-seen peer) (get-universal-time))
          
          ;; Perform Bitcoin handshake
          (when (perform-handshake connection)
            (log:info "Successfully connected to peer ~A:~A" address port)
            peer)
          (progn
            (disconnect-peer connection)
            (setf (peer-connected-p peer) nil)
            nil))
      (error (e)
        (log:error "Failed to connect to peer ~A:~A: ~A" address port e)
        (setf (peer-connected-p peer) nil)
        nil))))

(defun disconnect-peer (address port)
  "Disconnect from a peer"
  (let ((peer (get-peer address port)))
    (when peer
      (when (peer-connection peer)
        (disconnect-peer (peer-connection peer))
        (setf (peer-connection peer) nil))
      (setf (peer-connected-p peer) nil))))

(defun peer-alive-p (peer)
  "Check if peer is alive and connected"
  (and (peer-connected-p peer)
       (peer-connection peer)
       (connection-alive-p (peer-connection peer))))

(defun send-message-to-peer (address port message)
  "Send message to specific peer"
  (let ((peer (get-peer address port)))
    (when (and peer (peer-alive-p peer))
      (send-message (peer-connection peer) message))))

(defun receive-message-from-peer (address port &key (timeout 60))
  "Receive message from specific peer"
  (let ((peer (get-peer address port)))
    (when (and peer (peer-alive-p peer))
      (receive-message (peer-connection peer) :timeout timeout))))

(defun ping-peer (address port)
  "Ping a peer"
  (let ((peer (get-peer address port)))
    (when (and peer (peer-alive-p peer))
      (let ((nonce (ping-peer (peer-connection peer))))
        (setf (peer-last-ping peer) nonce)
        nonce))))

(defun pong-peer (address port nonce)
  "Pong a peer"
  (let ((peer (get-peer address port)))
    (when (and peer (peer-alive-p peer))
      (pong-peer (peer-connection peer) nonce)
      (setf (peer-last-pong peer) nonce))))

(defun request-blocks-from-peer (address port block-hashes)
  "Request blocks from specific peer"
  (let ((peer (get-peer address port)))
    (when (and peer (peer-alive-p peer))
      (request-blocks (peer-connection peer) block-hashes))))

(defun request-headers-from-peer (address port block-locator-hashes hash-stop)
  "Request block headers from specific peer"
  (let ((peer (get-peer address port)))
    (when (and peer (peer-alive-p peer))
      (request-headers (peer-connection peer) block-locator-hashes hash-stop))))

(defun get-connected-peers ()
  "Get list of connected peers"
  (let ((connected '()))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (when (peer-alive-p peer)
                 (push peer connected)))
             *peer-database*)
    connected))

(defun get-peer-count ()
  "Get number of connected peers"
  (length (get-connected-peers)))

(defun cleanup-dead-peers ()
  "Remove dead peers from database"
  (let ((to-remove '()))
    (maphash (lambda (key peer)
               (unless (peer-alive-p peer)
                 (push key to-remove)))
             *peer-database*)
    (dolist (key to-remove)
      (remhash key *peer-database*))))

(defun update-peer-info (address port &key services version height user-agent)
  "Update peer information"
  (let ((peer (get-peer address port)))
    (when peer
      (when services (setf (peer-services peer) services))
      (when version (setf (peer-version peer) version))
      (when height (setf (peer-height peer) height))
      (when user-agent (setf (peer-user-agent peer) user-agent))
      (setf (peer-last-seen peer) (get-universal-time)))))

(defun get-best-peer ()
  "Get the peer with the highest block height"
  (let ((best-peer nil)
        (best-height 0))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (when (and (peer-alive-p peer)
                          (> (peer-height peer) best-height))
                 (setf best-peer peer
                       best-height (peer-height peer))))
             *peer-database*)
    best-peer))

(defun broadcast-message (message)
  "Broadcast message to all connected peers"
  (let ((sent-count 0))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (when (peer-alive-p peer)
                 (when (send-message (peer-connection peer) message)
                   (incf sent-count))))
             *peer-database*)
    sent-count))

(defun peer-stats ()
  "Get peer statistics"
  (let ((total 0)
        (connected 0)
        (total-height 0)
        (max-height 0))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (incf total)
               (when (peer-alive-p peer)
                 (incf connected)
                 (incf total-height (peer-height peer))
                 (setf max-height (max max-height (peer-height peer)))))
             *peer-database*)
    (list :total total
          :connected connected
          :average-height (if (> connected 0) (/ total-height connected) 0)
          :max-height max-height)))
