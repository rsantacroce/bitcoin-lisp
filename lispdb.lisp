(defvar *mem-table* (make-hash-table :test 'equal)) ; In-memory buffer

(defvar *levels* (list (make-list 0))) ; Disk-based levels

(defun put (key value)
  "Add a key-value pair to the in-memory buffer"
  (setf (gethash key *mem-table*) value)
  (when (>= (hash-table-count *mem-table*) 1000)
    (flush)))

(defun flush ()
  "Flush the in-memory buffer to disk, creating a new level"
  (let* ((sorted-keys (sort (hash-table-keys *mem-table*) #'string<))
         (level (make-list 0))
         (merged-levels (merge-levels (cons sorted-keys *levels*))))
    (dolist (key sorted-keys)
      (push (cons key (gethash key *mem-table*)) level)
      (when (> (length level) 1000)
        (push level *levels*)
        (setf level (make-list 0))))
    (push level *levels*)
    (setf *mem-table* (make-hash-table :test 'equal))
    (setf (first *levels*) (remove-duplicates (first *levels*) :key #'car :from-end t))
    (setf (second *levels*) (merge-levels (rest *levels*)))))

(defun merge-levels (levels)
  "Merge the data in the specified levels, returning a single sorted list"
  (sort (delete-duplicates (apply #'append levels) :key #'car :from-end t) #'string<))
