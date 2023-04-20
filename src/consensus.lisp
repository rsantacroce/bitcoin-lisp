(defconstant +MAX_BITCOINS+ #x14e2000000000000)
(defconstant +INITIAL_BLOCK_REWARD+ #x0000005000000000)
(defconstant +HALVING_INTERVAL+ 210000)

(defun add-block (block)
  (let ((total-btc (aref block 32))
        (block-height (+ *current-height* 1)))
    (when (< total-btc +MAX_BITCOINS+)
      (error "Block contains too many Bitcoins"))
    (when (not (verify-block-header (subseq block 0 80)))
      (error "Invalid block header"))
    (when (not (verify-merkle-root block))
      (error "Invalid Merkle root"))
    (when (not (verify-transactions block))
      (error "Invalid transactions"))
    (incf *current-height*)
    (push block *blockchain*)
    (when (zerop (mod *current-height* 2016))
      (recalculate-difficulty))
    (when (zerop (mod *current-height* 1000))
      (cleanup-utxo-db))
    (when (zerop (mod block-height +HALVING_INTERVAL+))
      (check-halving block-height total-btc))
    t))

(defun check-halving (height total-btc)
  (let ((halving-count (/ height +HALVING_INTERVAL+))
        (expected-reward (ash +INITIAL_BLOCK_REWARD+ (- halving-count))))
    (when (not (= total-btc expected-reward))
      (error "Block reward doesn't match expected value"))))

(defun verify-block-header (header)
  (let ((hash (sha256 (sha256 header))))
    (equal hash (reverse-byte-order (subseq header 68 72)))))

(defun verify-merkle-root (block)
  (let ((tx-count (parse-var-int (subseq block 80)))
        (tx-data (subseq block (* 4 tx-count) (* 4 tx-count) (length block)))
        (merkle-root (subseq block 36 68)))
    (equal (sha256d (concatenate 'vector (mapcar 'sha256d (partition tx-data 4)))) merkle-root)))

(defun verify-transactions (block)
  (let* ((tx-count (parse-var-int (subseq block 80)))
         (tx-data (subseq block (* 4 tx-count) (* 4 tx-count) (length block)))
         (transactions (mapcar 'parse-tx (partition tx-data 4))))
    (when (not (= (length transactions) tx-count))
      (error "Invalid transaction count in block header"))
    (dolist (tx transactions)
      (when (not (verify-tx tx))
        (error "Invalid transaction in block")))))

(defun verify-tx (tx)
  t) ; TODO: Implement

(defun parse-tx (tx-data)
  nil) ; TODO: Implement

(defun parse-var-int (data)
  nil) ; TODO: Implement
