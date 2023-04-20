(defpackage #:bitcoin-lisp-consensus
  (:use :cl)
  (:import-from #:bitcoin-lisp-chainparams #:chain-params)
  (:import-from #:bitcoin-lisp-validation #:validate-transaction
                                          #:validate-block)
  (:export #:process-block
           #:process-transaction))
(in-package :bitcoin-lisp-consensus)

(defun find-previous-block (block blockchain)
  ;; TODO: Implement the function to find the previous block in the blockchain.
  )

(defun block-height (block)
  ;; TODO: Implement the function to return the height of a block.
  )

(defun update-block-height (block height)
  ;; TODO: Implement the function to update the height of a block.
  )

(defun remove-from-mempool (mempool tx)
  ;; TODO: Implement the function to remove a transaction from the mempool.
  )

(defun update-best-block (blockchain block)
  ;; TODO: Implement the function to update the best block in the blockchain.
  )

(defun process-block (block chain-params blockchain mempool)
  "Process a block and update the blockchain state."
  (unless (validate-block block chain-params)
    (error "Invalid block"))
  (let ((prev-block (find-previous-block block blockchain)))
    (unless prev-block
      (error "Previous block not found in the blockchain"))
    (let ((height (1+ (block-height prev-block))))
      (update-block-height block height)
      (dolist (tx (block-transactions block))
        (remove-from-mempool mempool tx))
      (update-blockchain-state blockchain block)
      (update-best-block blockchain block))))

(defun process-transaction (transaction chain-params mempool blockchain)
  "Process a transaction and update the mempool and blockchain state."
  (unless (validate-transaction transaction chain-params mempool)
    (error "Invalid transaction"))
  (if (coinbase-transaction-p transaction)
    (error "Coinbase transactions can only be included in a block")
    (progn
      (add-to-mempool mempool transaction)
      (remove-conflicting-transactions mempool transaction))))
