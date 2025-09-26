(in-package :bitcoin-crawler.protocol)

;; Bitcoin protocol serialization functions
;; Handles variable-length integers, strings, and other Bitcoin-specific data types

(defun serialize-varint (value)
  "Serialize a variable-length integer (varint) as per Bitcoin protocol"
  (cond
    ((< value #xFD)
     (vector (logand value #xFF)))
    ((<= value #xFFFF)
     (concatenate 'vector #(#xFD) (serialize-uint16 value)))
    ((<= value #xFFFFFFFF)
     (concatenate 'vector #(#xFE) (serialize-uint32 value)))
    (t
     (concatenate 'vector #(#xFF) (serialize-uint64 value)))))

(defun deserialize-varint (stream)
  "Deserialize a variable-length integer from stream"
  (let ((first-byte (read-byte stream)))
    (cond
      ((< first-byte #xFD) first-byte)
      ((= first-byte #xFD) (deserialize-uint16 stream))
      ((= first-byte #xFE) (deserialize-uint32 stream))
      ((= first-byte #xFF) (deserialize-uint64 stream))
      (t (error "Invalid varint prefix: ~A" first-byte)))))

(defun serialize-varstr (string)
  "Serialize a variable-length string"
  (let ((bytes (babel:string-to-octets string :encoding :utf-8)))
    (concatenate 'vector (serialize-varint (length bytes)) bytes)))

(defun deserialize-varstr (stream)
  "Deserialize a variable-length string from stream"
  (let ((length (deserialize-varint stream)))
    (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (babel:octets-to-string bytes :encoding :utf-8))))

(defun serialize-uint8 (value)
  "Serialize an 8-bit unsigned integer"
  (vector (logand value #xFF)))

(defun deserialize-uint8 (stream)
  "Deserialize an 8-bit unsigned integer"
  (read-byte stream))

(defun serialize-uint16 (value)
  "Serialize a 16-bit unsigned integer (little-endian)"
  (vector (logand value #xFF)
          (logand (ash value -8) #xFF)))

(defun deserialize-uint16 (stream)
  "Deserialize a 16-bit unsigned integer (little-endian)"
  (let ((low (read-byte stream))
        (high (read-byte stream)))
    (+ low (ash high 8))))

(defun serialize-uint32 (value)
  "Serialize a 32-bit unsigned integer (little-endian)"
  (vector (logand value #xFF)
          (logand (ash value -8) #xFF)
          (logand (ash value -16) #xFF)
          (logand (ash value -24) #xFF)))

(defun deserialize-uint32 (stream)
  "Deserialize a 32-bit unsigned integer (little-endian)"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (+ b0 (ash b1 8) (ash b2 16) (ash b3 24))))

(defun serialize-uint64 (value)
  "Serialize a 64-bit unsigned integer (little-endian)"
  (vector (logand value #xFF)
          (logand (ash value -8) #xFF)
          (logand (ash value -16) #xFF)
          (logand (ash value -24) #xFF)
          (logand (ash value -32) #xFF)
          (logand (ash value -40) #xFF)
          (logand (ash value -48) #xFF)
          (logand (ash value -56) #xFF)))

(defun deserialize-uint64 (stream)
  "Deserialize a 64-bit unsigned integer (little-endian)"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream))
        (b4 (read-byte stream))
        (b5 (read-byte stream))
        (b6 (read-byte stream))
        (b7 (read-byte stream)))
    (+ b0 (ash b1 8) (ash b2 16) (ash b3 24)
       (ash b4 32) (ash b5 40) (ash b6 48) (ash b7 56))))

(defun serialize-uint256 (value)
  "Serialize a 256-bit unsigned integer (little-endian)"
  (let ((result (make-array 32 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 32
          do (setf (aref result i) (logand value #xFF)
                   value (ash value -8)))
    result))

(defun deserialize-uint256 (stream)
  "Deserialize a 256-bit unsigned integer (little-endian)"
  (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (let ((result 0))
      (loop for i from 31 downto 0
            do (setf result (+ (ash result 8) (aref bytes i))))
      result)))

(defun serialize-uint160 (value)
  "Serialize a 160-bit unsigned integer (little-endian)"
  (let ((result (make-array 20 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 20
          do (setf (aref result i) (logand value #xFF)
                   value (ash value -8)))
    result))

(defun deserialize-uint160 (stream)
  "Deserialize a 160-bit unsigned integer (little-endian)"
  (let ((bytes (make-array 20 :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (let ((result 0))
      (loop for i from 19 downto 0
            do (setf result (+ (ash result 8) (aref bytes i))))
      result)))

(defun serialize-ipv6 (ipv6-address)
  "Serialize an IPv6 address"
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 16
          do (setf (aref bytes i) (aref ipv6-address i)))
    bytes))

(defun deserialize-ipv6 (stream)
  "Deserialize an IPv6 address"
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

(defun serialize-ipv4 (ipv4-address)
  "Serialize an IPv4 address"
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 4
          do (setf (aref bytes i) (aref ipv4-address i)))
    bytes))

(defun deserialize-ipv4 (stream)
  "Deserialize an IPv4 address"
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

;; Bitcoin-specific serialization
(defun serialize-coinbase (height)
  "Serialize coinbase transaction height"
  (let ((script (make-array (+ 1 (ceiling (log height 256) 1)) :element-type '(unsigned-byte 8))))
    (setf (aref script 0) (ceiling (log height 256) 1))
    (let ((temp height))
      (loop for i from 1 below (length script)
            do (setf (aref script i) (logand temp #xFF)
                     temp (ash temp -8))))
    script))

(defun serialize-script (script)
  "Serialize a Bitcoin script"
  (concatenate 'vector (serialize-varint (length script)) script))

(defun deserialize-script (stream)
  "Deserialize a Bitcoin script"
  (let ((length (deserialize-varint stream)))
    (let ((script (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence script stream)
      script)))

;; Utility functions
(defun bytes-to-hex (bytes)
  "Convert bytes to hexadecimal string"
  (format nil "~{~2,'0X~}" (coerce bytes 'list)))

(defun hex-to-bytes (hex-string)
  "Convert hexadecimal string to bytes"
  (let ((len (length hex-string)))
    (when (oddp len)
      (error "Hex string length must be even"))
    (let ((bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
      (loop for i from 0 below len by 2
            for j from 0
            do (setf (aref bytes j)
                     (parse-integer (subseq hex-string i (+ i 2)) :radix 16)))
      bytes)))

(defun reverse-bytes (bytes)
  "Reverse byte order"
  (let ((reversed (make-array (length bytes) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length bytes)
          do (setf (aref reversed i) (aref bytes (- (length bytes) 1 i))))
    reversed))
