(in-package :bitcoin)

;; Importing the external library 'alexandria' to use the 'chunk' function
(require :alexandria)

(defun sha256 (bytes)
  "Calculates the SHA-256 hash of a sequence of bytes."
  (let* ((block-size 64)
         (message-length (length bytes))
         (message-bit-length (* 8 message-length))
         (padding-start (if (< (mod (+ message-length 9) block-size) 4)
                             (+ message-length 9 block-size)
                             (+ message-length 9 (mod (- block-size (mod (+ message-length 9) block-size)) block-size))))
         (padding-end (- padding-start message-length))
         (padded-bytes (append bytes (make-list padding-end :initial-element 0)
                               (list (logand #xff (ash message-bit-length -56))
                                     (logand #xff (ash message-bit-length -48))
                                     (logand #xff (ash message-bit-length -40))
                                     (logand #xff (ash message-bit-length -32))
                                     (logand #xff (ash message-bit-length -24))
                                     (logand #xff (ash message-bit-length -16))
                                     (logand #xff (ash message-bit-length -8))
                                     (logand #xff message-bit-length))))
         (chunks (chunk padded-bytes block-size))
         (h0 0x6a09e667)
         (h1 0xbb67ae85)
         (h2 0x3c6ef372)
         (h3 0xa54ff53a)
         (h4 0x510e527f)
         (h5 0x9b05688c)
         (h6 0x1f83d9ab)
         (h7 0x5be0cd19)
         (k '(0x428a2f98 0x71374491 0xb5c0fbcf 0xe9b5dba5 0x3956c25b 0x59f111f1 0x923f82a4 0xab1c5ed5
              0xd807aa98 0x12835b01 0x243185be 0x550c7dc3 0x72be5d74 0x80deb1fe 0x9bdc06a7 0xc19bf174
              0xe49b69c1 0xefbe4786 0x0fc19dc6 0x240ca1cc 0x2de92c6f 0x4a7484aa 0x5cb0a9dc 0x76f988da
              0x983e5152 0xa831c66d 0xb00327c8 0xbf597fc7 0xc6e00bf3 0xd5a79147 0x06ca6351 0x14292967
              0x27b70a85 0x2e1b2138 0x4d2c6dfc 0x53380d13 0x650a7354 0x766a0abb 0x81c2c92e 0x92722c85
              0xa2bfe8a1 0xa81a664b 0xc24b8b70 0xc76c51a3 
