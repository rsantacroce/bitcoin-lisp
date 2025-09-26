(in-package :bitcoin-crawler.protocol)

;; Bitcoin cryptographic functions
;; Note: In a real implementation, you would use a proper crypto library
;; For this example, we'll implement basic SHA256 using available libraries

(defun sha256 (data)
  "Compute SHA256 hash of data"
  (let ((bytes (if (stringp data)
                   (babel:string-to-octets data)
                   data)))
    (crypto:digest :sha256 bytes)))

(defun double-sha256 (data)
  "Compute double SHA256 hash (SHA256(SHA256(data)))"
  (sha256 (sha256 data)))

(defun hash160 (data)
  "Compute RIPEMD160(SHA256(data))"
  (let ((sha256-hash (sha256 data)))
    (crypto:digest :ripemd160 sha256-hash)))

(defun hash-to-hex (hash)
  "Convert hash bytes to hexadecimal string"
  (format nil "~{~2,'0X~}" (coerce hash 'list)))

(defun hex-to-hash (hex-string)
  "Convert hexadecimal string to hash bytes"
  (let ((len (length hex-string)))
    (when (oddp len)
      (error "Hex string length must be even"))
    (let ((bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
      (loop for i from 0 below len by 2
            for j from 0
            do (setf (aref bytes j)
                     (parse-integer (subseq hex-string i (+ i 2)) :radix 16)))
      bytes)))

(defun reverse-hash (hash)
  "Reverse byte order of hash (Bitcoin uses little-endian)"
  (let ((reversed (make-array (length hash) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length hash)
          do (setf (aref reversed i) (aref hash (- (length hash) 1 i))))
    reversed))

(defun hash-equals (hash1 hash2)
  "Compare two hashes for equality"
  (and (= (length hash1) (length hash2))
       (every #'= hash1 hash2)))

;; Bitcoin-specific hash functions
(defun bitcoin-hash (data)
  "Bitcoin double SHA256 hash with reversed output"
  (reverse-hash (double-sha256 data)))

(defun merkle-root (tx-hashes)
  "Calculate Merkle root from transaction hashes"
  (if (= (length tx-hashes) 1)
      (first tx-hashes)
      (let ((next-level '()))
        (loop for i from 0 below (length tx-hashes) by 2
              do (let ((left (aref tx-hashes i))
                       (right (if (< (1+ i) (length tx-hashes))
                                  (aref tx-hashes (1+ i))
                                  left)))
                   (push (double-sha256 (concatenate 'vector left right))
                         next-level)))
        (merkle-root (reverse next-level)))))

;; Address generation (for future use)
(defun pubkey-to-address (pubkey &optional (testnet nil))
  "Convert public key to Bitcoin address"
  (let* ((hash160 (hash160 pubkey))
         (version (if testnet #x6F #x00))
         (payload (concatenate 'vector (vector version) hash160))
         (checksum (subseq (double-sha256 payload) 0 4))
         (address-bytes (concatenate 'vector payload checksum)))
    (base58-encode address-bytes)))

(defun base58-encode (bytes)
  "Encode bytes to Base58 (simplified implementation)"
  ;; This is a simplified implementation
  ;; In production, use a proper Base58 library
  (let ((alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
        (result ""))
    (loop for byte across bytes
          do (setf result (concatenate 'string result (string (char alphabet (mod byte 58)))))
          finally (return result))))
