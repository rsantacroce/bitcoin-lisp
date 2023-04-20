(in-package :cl-user)
(defpackage #:bitcoin-lisp-main
  (:use :cl :bitcoin-lisp)
  (:import-from #:bitcoin-lisp-tests :run-all-tests))
(in-package :bitcoin-lisp-main)

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((command (if args (first args) "help")))
    (case (intern (string-upcase command) :keyword)
      (:HELP (format t "Usage: bitcoin-lisp [command]~%Commands:~%  help    - Show this help message~%  test    - Run tests~%"))
      (:TEST (progn
               (asdf:load-system :bitcoin-lisp-tests)
               (rove:run :bitcoin-lisp-tests)))
      (t (format t "Unknown command. Type 'bitcoin-lisp help' for usage instructions.~%")))))

;; Uncomment the following line to run the main function when the file is loaded.
;; (main)
