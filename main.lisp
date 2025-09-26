(in-package :bitcoin-crawler)

;; Bitcoin Crawler Main Entry Point

(defun main (&rest args)
  "Main entry point for Bitcoin crawler"
  (let ((command (first args))
        (options (rest args)))
    (case (intern (string-upcase (or command "help")) :keyword)
      (:start
       (start-crawler-command options))
      (:sync
       (sync-blockchain-command options))
      (:status
       (show-status-command options))
      (:peers
       (show-peers-command options))
      (:chain
       (show-chain-command options))
      (:stop
       (stop-crawler-command options))
      (:help
       (show-help-command))
      (t
       (format t "Unknown command: ~A~%" command)
       (show-help-command)))))

(defun start-crawler-command (options)
  "Start crawler command"
  (let ((database-path "./bitcoin-data")
        (max-peers 8))
    (dolist (option options)
      (let ((key (first option))
            (value (second option)))
        (case (intern (string-upcase key) :keyword)
          (:database-path (setf database-path value))
          (:max-peers (setf max-peers (parse-integer value))))))
    
    (format t "Starting Bitcoin crawler...~%")
    (format t "Database path: ~A~%" database-path)
    (format t "Max peers: ~A~%" max-peers)
    
    (start-crawler :database-path database-path :max-peers max-peers)))

(defun sync-blockchain-command (options)
  "Sync blockchain command"
  (let ((database-path "./bitcoin-data")
        (target-height 0)
        (max-peers 8))
    (dolist (option options)
      (let ((key (first option))
            (value (second option)))
        (case (intern (string-upcase key) :keyword)
          (:database-path (setf database-path value))
          (:target-height (setf target-height (parse-integer value)))
          (:max-peers (setf max-peers (parse-integer value))))))
    
    (format t "Synchronizing blockchain...~%")
    (format t "Database path: ~A~%" database-path)
    (format t "Target height: ~A~%" target-height)
    (format t "Max peers: ~A~%" max-peers)
    
    (sync-blockchain :database-path database-path 
                     :target-height target-height 
                     :max-peers max-peers)))

(defun show-status-command (options)
  "Show status command"
  (declare (ignore options))
  (show-status))

(defun show-peers-command (options)
  "Show peers command"
  (declare (ignore options))
  (show-peers))

(defun show-chain-command (options)
  "Show chain command"
  (declare (ignore options))
  (show-chain-info))

(defun stop-crawler-command (options)
  "Stop crawler command"
  (declare (ignore options))
  (format t "Stopping Bitcoin crawler...~%")
  (stop-crawler))

(defun show-help-command ()
  "Show help command"
  (format t "Bitcoin Crawler - Bitcoin Network Client~%")
  (format t "========================================~%")
  (format t "~%")
  (format t "Usage: bitcoin-crawler <command> [options]~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  start     Start the Bitcoin crawler~%")
  (format t "  sync      Synchronize the blockchain~%")
  (format t "  status    Show crawler status~%")
  (format t "  peers     Show connected peers~%")
  (format t "  chain     Show blockchain information~%")
  (format t "  stop      Stop the crawler~%")
  (format t "  help      Show this help message~%")
  (format t "~%")
  (format t "Options:~%")
  (format t "  --database-path <path>    Database storage path (default: ./bitcoin-data)~%")
  (format t "  --max-peers <number>      Maximum number of peer connections (default: 8)~%")
  (format t "  --target-height <number>  Target blockchain height (default: 0 = sync to tip)~%")
  (format t "~%")
  (format t "Examples:~%")
  (format t "  bitcoin-crawler start --database-path /tmp/bitcoin --max-peers 16~%")
  (format t "  bitcoin-crawler sync --target-height 100000~%")
  (format t "  bitcoin-crawler status~%")
  (format t "  bitcoin-crawler peers~%")
  (format t "  bitcoin-crawler chain~%")
  (format t "  bitcoin-crawler stop~%"))

;; Interactive REPL functions
(defun repl-start ()
  "Start interactive REPL"
  (format t "Bitcoin Crawler Interactive Mode~%")
  (format t "================================~%")
  (format t "Type 'help' for available commands~%")
  (format t "~%")
  
  (loop
    (format t "bitcoin> ")
    (force-output)
    (let ((input (read-line)))
      (when (string= input "quit")
        (return))
      (handle-repl-command input))))

(defun handle-repl-command (input)
  "Handle REPL command"
  (let ((parts (split-sequence:split-sequence #\Space input :remove-empty-subseqs t)))
    (if parts
        (let ((command (first parts))
              (args (rest parts)))
          (case (intern (string-upcase command) :keyword)
            (:start
             (start-crawler))
            (:sync
             (let ((target-height (if args (parse-integer (first args)) 0)))
               (sync-blockchain :target-height target-height)))
            (:status
             (show-status))
            (:peers
             (show-peers))
            (:chain
             (show-chain-info))
            (:stop
             (stop-crawler))
            (:help
             (show-repl-help))
            (:quit
             (format t "Goodbye!~%"))
            (t
             (format t "Unknown command: ~A~%" command)
             (show-repl-help)))
          (format t "~%"))
        (format t "~%"))))

(defun show-repl-help ()
  "Show REPL help"
  (format t "Available commands:~%")
  (format t "  start [target-height]  - Start crawler (optionally sync to height)~%")
  (format t "  sync [target-height]   - Sync blockchain to height~%")
  (format t "  status                 - Show crawler status~%")
  (format t "  peers                  - Show connected peers~%")
  (format t "  chain                  - Show blockchain info~%")
  (format t "  stop                   - Stop crawler~%")
  (format t "  help                   - Show this help~%")
  (format t "  quit                   - Exit REPL~%"))

;; Configuration management
(defun load-config-file (config-file)
  "Load configuration from file"
  (when (probe-file config-file)
    (load-config config-file)
    (format t "Loaded configuration from ~A~%" config-file)))

(defun save-config-file (config-file)
  "Save configuration to file"
  (with-open-file (stream config-file :direction :output :if-exists :overwrite)
    (maphash (lambda (key value)
               (format stream "~A=~A~%" key value))
             *config*))
  (format t "Saved configuration to ~A~%" config-file))

;; Logging setup
(defun setup-logging (&key (level :info) (file "./bitcoin-crawler.log"))
  "Setup logging configuration"
  (log:config :level level :file file)
  (log:info "Logging configured - Level: ~A, File: ~A" level file))

;; Main function for executable
(defun main-executable ()
  "Main function for standalone executable"
  (setup-logging)
  (load-config)
  (main))

;; Entry point for ASDF system
(defun run-bitcoin-crawler (&rest args)
  "Run Bitcoin crawler with arguments"
  (apply #'main args))

;; Export main functions
(export '(main main-executable run-bitcoin-crawler repl-start))
