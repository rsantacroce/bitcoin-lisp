(defun test-put-and-get ()
  "Test adding data to the LSM-tree and retrieving it"
  (let ((lsm (make-instance 'log-structured-merge-tree)))
    (lsm:put "key1" "value1")
    (lsm:put "key2" "value2")
    (lsm:put "key3" "value3")
    (assert (equal "value1" (lsm:get "key1")))
    (assert (equal "value2" (lsm:get "key2")))
    (assert (equal "value3" (lsm:get "key3")))
    (assert (equal nil (lsm:get "nonexistent-key")))))

(defun test-compaction ()
  "Test data compaction in the LSM-tree"
  (let ((lsm (make-instance 'log-structured-merge-tree)))
    (dotimes (i 2000)
      (lsm:put (format "key~d" i) (format "value~d" i)))
    (assert (= (length (lsm:get-level 0)) 0)) ; In-memory buffer should be empty
    (assert (= (length (lsm:get-level 1)) 1000)) ; First level should have 1000 elements
    (assert (= (length (lsm:get-level 2)) 1000)) ; Second level should have 1000 elements
    (assert (= (length (lsm:get-level 3)) 0)) ; Third level should be empty
    (lsm:flush)
    (assert (= (length (lsm:get-level 0)) 1000)) ; In-memory buffer should have been flushed
    (assert (= (length (lsm:get-level 1)) 1000)) ; First level should still have 1000 elements
    (assert (= (length (lsm:get-level 2)) 1000)) ; Second level should still have 1000 elements
    (assert (= (length (lsm:get-level 3)) 0)) ; Third level should still be empty
    (dotimes (i 1000)
      (lsm:get (format "key~d" i))) ; Read first 1000 elements
    (assert (= (length (lsm:get-level 0)) 0)) ; In-memory buffer should be empty
    (assert (= (length (lsm:get-level 1)) 0)) ; First level should be empty
    (assert (= (length (lsm:get-level 2)) 2000)) ; Second level should have been compacted to 2000 elements
    (assert (= (length (lsm:get-level 3)) 0)) ; Third level should still be empty)))

(defun test-delete ()
  "Test deleting data from the LSM-tree"
  (let ((lsm (make-instance 'log-structured-merge-tree)))
    (lsm:put "key1" "value1")
    (lsm:put "key2" "value2")
    (lsm:put "key3" "value3")
    (assert (equal "value1" (lsm:get "key1")))
    (assert (equal "value2" (lsm:get "key2")))
    (assert (equal "value3" (lsm:get "key3")))
    (lsm:delete "key2")
    (assert (equal nil (lsm:get "key2")))
    (assert (= (length (lsm:get-level 0)) 0)) ; In-memory buffer should be empty
    (assert (= (length (lsm:get-level 1)) 2)) ; First level should have 2 elements
    (assert (= (length (lsm:get-level 2)) 0)) ; Second level should be empty
    (assert (= (length (lsm:get-level 3)) 0)) ; Third level should be empty)))

(defun test-read-write ()
  "Test reading and writing large amounts of data to the LSM-tree"
  (let ((lsm (make-instance 'log-structured-merge-tree)))
    (dotimes (i 100000)
      (lsm:put (format "key~d" i) (make-string 1000 ?x))) ; Add 100000 keys with 1000-character values
    (assert (= (length (lsm:get-level 0)) 0)) ; In-memory buffer should be empty
    (assert (= (length (lsm:get-level 1)) 1000)) ; First level should have 1000 elements
    (assert (= (length (lsm:get-level 2)) 1000)) ; Second level should have 1000 elements
    (assert (= (length (lsm:get-level 3)) 98)) ; Third level should have 98 elements
    (dotimes (i 100000)
      (assert (equal (make-string 1000 ?x) (lsm:get (format "key~d" i))))))) ; Read all 100000 keys
