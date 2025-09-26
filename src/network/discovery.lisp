(in-package :bitcoin-crawler.network)

;; Bitcoin peer discovery

(defvar *seed-nodes* '("seed.bitcoin.sipa.be:8333"
                       "dnsseed.bluematt.me:8333"
                       "dnsseed.bitcoin.dashjr.org:8333"
                       "seed.bitcoinstats.com:8333"
                       "seed.bitcoin.jonasschnelli.ch:8333"))

(defun parse-address-port (address-string)
  "Parse address:port string into address and port"
  (let ((colon-pos (position #\: address-string)))
    (if colon-pos
        (values (subseq address-string 0 colon-pos)
                (parse-integer (subseq address-string (1+ colon-pos))))
        (values address-string 8333))))

(defun connect-to-seeds (&key (max-connections 8) (timeout 30))
  "Connect to Bitcoin seed nodes"
  (let ((connected-count 0)
        (failed-seeds '()))
    
    (dolist (seed *seed-nodes*)
      (when (< connected-count max-connections)
        (multiple-value-bind (address port) (parse-address-port seed)
          (handler-case
              (progn
                (log:info "Connecting to seed node ~A:~A" address port)
                (when (connect-to-peer address port :timeout timeout)
                  (incf connected-count)
                  (log:info "Successfully connected to seed ~A:~A" address port)))
            (error (e)
              (log:warn "Failed to connect to seed ~A:~A: ~A" address port e)
              (push seed failed-seeds))))))
    
    (log:info "Connected to ~A seed nodes" connected-count)
    (when failed-seeds
      (log:warn "Failed to connect to ~A seed nodes: ~A" (length failed-seeds) failed-seeds))
    
    connected-count))

(defun discover-peers-from-seed (address port &key (timeout 30))
  "Discover peers from a seed node"
  (let ((peer (connect-to-peer address port :timeout timeout)))
    (when peer
      (handler-case
          (progn
            ;; Perform handshake
            (perform-handshake peer)
            
            ;; Request peer addresses (this is a simplified version)
            ;; In a real implementation, you would send getaddr messages
            (log:info "Peer discovery completed with ~A:~A" address port)
            t)
        (error (e)
          (log:error "Error during peer discovery with ~A:~A: ~A" address port e)
          (disconnect-peer peer)
          nil)))))

(defun discover-peers (&key (max-peers 50) (timeout 30))
  "Discover peers from seed nodes"
  (let ((discovered-count 0))
    (dolist (seed *seed-nodes*)
      (when (< discovered-count max-peers)
        (multiple-value-bind (address port) (parse-address-port seed)
          (when (discover-peers-from-seed address port :timeout timeout)
            (incf discovered-count)))))
    (log:info "Discovered ~A peers" discovered-count)
    discovered-count))

(defun maintain-peer-connections (&key (min-peers 4) (max-peers 8))
  "Maintain peer connections within specified range"
  (let ((current-count (get-peer-count)))
    (cond
      ((< current-count min-peers)
       (log:info "Low peer count (~A), attempting to connect to more peers" current-count)
       (connect-to-seeds :max-connections (- max-peers current-count)))
      ((> current-count max-peers)
       (log:info "High peer count (~A), cleaning up excess connections" current-count)
       (cleanup-excess-peers (- current-count max-peers)))
      (t
       (log:debug "Peer count is within range (~A)" current-count)))))

(defun cleanup-excess-peers (excess-count)
  "Remove excess peer connections"
  (let ((peers (get-connected-peers))
        (removed 0))
    (dolist (peer peers)
      (when (< removed excess-count)
        (let ((address (peer-address peer))
              (port (peer-port peer)))
          (disconnect-peer address port)
          (incf removed))))))

(defun ping-all-peers ()
  "Ping all connected peers"
  (let ((pinged-count 0))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (when (peer-alive-p peer)
                 (ping-peer (peer-address peer) (peer-port peer))
                 (incf pinged-count)))
             *peer-database*)
    (log:debug "Pinged ~A peers" pinged-count)
    pinged-count))

(defun handle-peer-messages ()
  "Handle incoming messages from all peers"
  (let ((message-count 0))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (when (peer-alive-p peer)
                 (let ((message (receive-message-from-peer (peer-address peer) (peer-port peer) :timeout 0.1)))
                   (when message
                     (handle-peer-message peer message)
                     (incf message-count)))))
             *peer-database*)
    message-count))

(defun handle-peer-message (peer message)
  "Handle a message from a specific peer"
  (let ((command (bitcoin-crawler.protocol:message-command message)))
    (case (intern (string-upcase command) :keyword)
      (:ping
       (let ((nonce (bitcoin-crawler.protocol:ping-nonce message)))
         (pong-peer (peer-address peer) (peer-port peer) nonce)))
      (:pong
       (let ((nonce (bitcoin-crawler.protocol:pong-nonce message)))
         (log:debug "Received pong ~A from ~A:~A" nonce (peer-address peer) (peer-port peer))))
      (:version
       (log:info "Received version ~A from ~A:~A" 
                 (bitcoin-crawler.protocol:version-version message)
                 (peer-address peer) (peer-port peer)))
      (:inv
       (log:debug "Received inv message from ~A:~A" (peer-address peer) (peer-port peer)))
      (:block
       (log:debug "Received block message from ~A:~A" (peer-address peer) (peer-port peer)))
      (:tx
       (log:debug "Received tx message from ~A:~A" (peer-address peer) (peer-port peer)))
      (t
       (log:debug "Received unknown message ~A from ~A:~A" command (peer-address peer) (peer-port peer))))))

(defun start-peer-manager ()
  "Start the peer management background task"
  (log:info "Starting peer manager")
  (bt:make-thread
   (lambda ()
     (loop
       (handler-case
           (progn
             (maintain-peer-connections)
             (cleanup-dead-peers)
             (ping-all-peers)
             (handle-peer-messages)
             (sleep 10))
         (error (e)
           (log:error "Error in peer manager: ~A" e)
           (sleep 5)))))))

(defun stop-peer-manager ()
  "Stop the peer management background task"
  (log:info "Stopping peer manager")
  (maphash (lambda (key peer)
             (declare (ignore key))
             (disconnect-peer (peer-address peer) (peer-port peer)))
           *peer-database*)
  (clrhash *peer-database*))

(defun get-peer-list ()
  "Get formatted list of all peers"
  (let ((peers '()))
    (maphash (lambda (key peer)
               (declare (ignore key))
               (push (list :address (peer-address peer)
                           :port (peer-port peer)
                           :connected (peer-alive-p peer)
                           :height (peer-height peer)
                           :version (peer-version peer)
                           :user-agent (peer-user-agent peer))
                     peers))
             *peer-database*)
    peers))
