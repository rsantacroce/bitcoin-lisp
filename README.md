# Bitcoin Network Crawler

A Common Lisp implementation of a Bitcoin network crawler that connects to Bitcoin peers, discovers the network, and downloads the complete blockchain to a local database.

## Overview

This project implements a Bitcoin peer-to-peer network client that:
- Connects to Bitcoin seed nodes
- Discovers and manages peer connections
- Downloads blocks and transactions from the network
- Stores blockchain data in a local database
- Provides a foundation for building a full Bitcoin node

## Features

### Network Layer
- Bitcoin P2P protocol implementation
- Connection management to multiple peers
- Peer discovery and maintenance
- IPv4 and IPv6 support
- Automatic reconnection and error handling

### Bitcoin Protocol
- Complete message serialization/deserialization
- Version handshake and peer negotiation
- Block and transaction downloading
- Inventory management
- Ping/pong keepalive

### Data Storage
- File-based key-value database
- Efficient block and transaction storage
- Block index for height-to-hash mapping
- UTXO set preparation
- Chain state management

### Synchronization
- Chronological blockchain sync from genesis
- Batch processing for efficiency
- Progress tracking and monitoring
- Orphan block handling
- Resume capability

## Requirements

### Dependencies
- Common Lisp implementation (SBCL, CCL, or similar)
- ASDF (included with most Common Lisp implementations)
- Required libraries:
  - `usocket` - Network socket library
  - `nibbles` - Byte manipulation
  - `crypto` - Cryptographic functions
  - `babel` - Character encoding
  - `flexi-streams` - Stream utilities
  - `cl-ppcre` - Regular expressions
  - `log4cl` - Logging framework

### Installation

1. Install required dependencies using Quicklisp:
```lisp
(ql:quickload '("usocket" "nibbles" "crypto" "babel" "flexi-streams" "cl-ppcre" "log4cl"))
```

2. Load the system:
```lisp
(asdf:load-system :bitcoin-crawler)
```

## Usage

### Command Line Interface

The crawler provides a command-line interface for easy operation:

```bash
# Start the crawler
bitcoin-crawler start --database-path ./bitcoin-data --max-peers 8

# Sync blockchain to specific height
bitcoin-crawler sync --target-height 100000

# Show current status
bitcoin-crawler status

# Show connected peers
bitcoin-crawler peers

# Show blockchain information
bitcoin-crawler chain

# Stop the crawler
bitcoin-crawler stop
```

### Interactive REPL

Start an interactive session:

```lisp
(bitcoin-crawler:repl-start)
```

Available commands:
- `start [target-height]` - Start crawler
- `sync [target-height]` - Sync blockchain
- `status` - Show status
- `peers` - Show peers
- `chain` - Show chain info
- `stop` - Stop crawler
- `help` - Show help
- `quit` - Exit

### Programmatic Usage

```lisp
;; Start the crawler
(let ((crawler (bitcoin-crawler:start-crawler 
                :database-path "./bitcoin-data" 
                :max-peers 8)))
  ;; Crawler runs in background
  (sleep 60) ; Let it run for a minute
  (bitcoin-crawler:stop-crawler))

;; Sync blockchain to specific height
(bitcoin-crawler:sync-blockchain 
 :database-path "./bitcoin-data" 
 :target-height 100000)

;; Get crawler status
(bitcoin-crawler:get-crawler-status)

;; Get block information
(bitcoin-crawler:get-block-info block-hash)

;; Get transaction information
(bitcoin-crawler:get-transaction-info tx-hash)
```

## Configuration

The crawler can be configured through a configuration file or programmatically:

```lisp
;; Set configuration values
(bitcoin-crawler:set-config "database-path" "./my-bitcoin-data")
(bitcoin-crawler:set-config "max-peers" 16)
(bitcoin-crawler:set-config "sync-batch-size" 1000)

;; Load configuration from file
(bitcoin-crawler:load-config "config.txt")

;; Save current configuration
(bitcoin-crawler:save-config-file "config.txt")
```

### Configuration Options

- `database-path` - Path to store blockchain data
- `max-peers` - Maximum number of peer connections
- `connection-timeout` - Connection timeout in seconds
- `message-timeout` - Message timeout in seconds
- `sync-batch-size` - Number of blocks to request per batch
- `log-level` - Logging level (:debug, :info, :warn, :error)
- `log-file` - Log file path

## Architecture

### Package Structure

- `bitcoin-crawler` - Main package and public API
- `bitcoin-crawler.protocol` - Bitcoin protocol implementation
- `bitcoin-crawler.network` - Network communication layer
- `bitcoin-crawler.storage` - Database and storage layer
- `bitcoin-crawler.sync` - Blockchain synchronization

### Key Components

1. **Protocol Layer** (`src/protocol/`)
   - Message serialization/deserialization
   - Cryptographic functions
   - Bitcoin-specific data types

2. **Network Layer** (`src/network/`)
   - Peer connection management
   - Message transmission
   - Peer discovery

3. **Storage Layer** (`src/storage/`)
   - Database operations
   - Data schema definitions
   - Persistence management

4. **Sync Layer** (`src/sync/`)
   - Blockchain synchronization
   - Progress tracking
   - Error handling

## Database Schema

The crawler uses a file-based key-value store with the following schema:

- **Blocks**: `1<block-hash>` → serialized block data
- **Transactions**: `2<tx-hash>` → serialized transaction data
- **Block Index**: `3<height>` → block hash
- **Chain Tip**: `4` → chain state
- **UTXOs**: `5<tx-hash><output-index>` → UTXO data
- **Metadata**: `6<key>` → metadata value

## Development

### Building

1. Load the system:
```lisp
(asdf:load-system :bitcoin-crawler)
```

2. Run tests (if available):
```lisp
(asdf:test-system :bitcoin-crawler)
```

### Adding Features

The codebase is designed to be extensible. Key areas for enhancement:

1. **Block Validation** - Add proper block and transaction validation
2. **Mempool Management** - Track unconfirmed transactions
3. **RPC Interface** - Add JSON-RPC API
4. **Wallet Integration** - Add wallet functionality
5. **Mining Support** - Add mining capabilities

### Testing

The crawler includes basic error handling and logging. For production use, additional testing and validation would be recommended.

## Limitations

This is a learning/research project with the following limitations:

- **No Block Validation** - Blocks are stored without validation
- **No Transaction Verification** - Transactions are not verified
- **No Consensus Rules** - Does not implement Bitcoin consensus
- **No Mempool** - Does not track unconfirmed transactions
- **No RPC Interface** - No external API provided
- **No Wallet** - No wallet functionality

## Security Considerations

- This software is for educational purposes only
- Do not use for production Bitcoin operations
- No security guarantees are provided
- Use at your own risk

## Contributing

Contributions are welcome! Areas for improvement:

1. **Code Quality** - Improve error handling and validation
2. **Performance** - Optimize database operations
3. **Features** - Add new functionality
4. **Documentation** - Improve documentation
5. **Testing** - Add comprehensive tests

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- Bitcoin Core developers for protocol specifications
- Common Lisp community for libraries and tools
- Bitcoin community for documentation and resources

## Support

For questions, issues, or contributions:

1. Check the documentation
2. Review the source code
3. Create an issue
4. Submit a pull request

## Changelog

### Version 0.1.0
- Initial implementation
- Basic network connectivity
- Block and transaction downloading
- File-based database storage
- Command-line interface
- Interactive REPL

## Future Plans

- [ ] Block and transaction validation
- [ ] Mempool management
- [ ] RPC interface
- [ ] Wallet integration
- [ ] Mining support
- [ ] Performance optimizations
- [ ] Comprehensive testing
- [ ] Documentation improvements
