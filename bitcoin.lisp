(in-package :bitcoin)

;; Importing the external library 'cl-base64' to be able to decode base64
(require :cl-base64)

;; Defining the genesis block as a string
(defparameter *genesis-block* "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c")

;; Decoding the genesis block
(defparameter *genesis-bytes* (map 'list #'parse-integer (coerce (base64-decode-string *genesis-block*) 'list)))

;; Extracting the message from the decoded genesis block
(defparameter *message* (subseq *genesis-bytes* 4 76))

(defun print-genesis-message ()
  (format t "Genesis Block: ~a~%Decoded Message: ~a~%" *genesis-bytes* *message*))
