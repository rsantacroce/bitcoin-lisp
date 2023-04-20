(defpackage #:bitcoin-lisp-consensus
  (:use :cl)
  (:import-from #:bitcoin-lisp-chainparams #:chain-params)
  (:import-from #:bitcoin-lisp-validation #:validate-transaction
                                          #:validate-block)
  (:export #:process-block
           #:process-transaction))
(in-package :bitcoin-lisp-consensus)

(defun process-block (block chain-params blockchain)
  "Process a block and update the blockchain state."
  (unless (validate-block block chain-params)
    (error "Invalid block"))
  (let ((prev-block (find-previous-block block blockchain)))
    (unless prev-block
      (error "Previous block not found in the blockchain"))
    (update-blockchain-state blockchain block)))

(defun process-transaction (transaction chain-params mempool blockchain)
  "Process a transaction and update the mempool and blockchain state."
  (unless (validate-transaction transaction chain-params mempool)
    (error "Invalid transaction"))
  (if (coinbase-transaction-p transaction)
    (error "Coinbase transactions can only be included in a block")
    (progn
      (add-to-mempool mempool transaction)
      (remove-conflicting-transactions mempool transaction))))
