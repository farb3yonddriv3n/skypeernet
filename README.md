# skypeernet

[![BSD license](https://img.shields.io/badge/License-GPL3.0-blue.svg)](https://opensource.org/licenses/GPL-3.0)
[![Build Status](https://travis-ci.org/farb3yonddriv3n/skypeernet.png?branch=master)](http://travis-ci.org/farb3yonddriv3n/skypeernet)
[![Ohloh Project Status](https://www.openhub.net/p/skypeernet/widgets/project_thin_badge.gif)](https://www.openhub.net/p/skypeernet)

**skypeernet** is a decentralized peer-to-peer file sharing network with blockchain-based file tracking.

## Table of Contents

- [Features](#features)
- [Architecture](#architecture)
- [Building](#building)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [CLI Commands Reference](#cli-commands-reference)
- [File Sharing Workflow](#file-sharing-workflow)
- [TCP Tunneling](#tcp-tunneling)
- [Plugins](#plugins)
- [Troubleshooting](#troubleshooting)

## Features

- **Decentralized P2P Network**: Direct peer-to-peer communication with tracker-assisted discovery
- **Blockchain-based File Index**: Files are tracked in a mineable blockchain
- **Chunked File Transfer**: Large files are split into chunks with SHA256 verification
- **RSA Authentication**: Peers authenticate using RSA key pairs
- **NAT Traversal**: Proxy/shadow routing for peers behind NAT
- **TCP Tunneling**: Tunnel TCP connections over the P2P network
- **Bandwidth Control**: Configurable upload/download limits
- **Plugin System**: Erlang (johnconnor) and Python (skynet) plugins

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         NETWORK                                  │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐      │
│  │ Tracker │◄──►│  Peer A │◄──►│  Peer B │◄──►│  Peer C │      │
│  └─────────┘    └─────────┘    └─────────┘    └─────────┘      │
│       │              │              │              │            │
│       └──────────────┴──────────────┴──────────────┘            │
│                    UDP Protocol (port 5775)                     │
└─────────────────────────────────────────────────────────────────┘

PEER INTERNALS:
┌─────────────────────────────────────────┐
│              Application                 │
│  ┌─────────────┐  ┌─────────────────┐   │
│  │  CLI (stdin)│  │  API (pipes)    │   │
│  └──────┬──────┘  └────────┬────────┘   │
│         └──────────┬───────┘            │
│              ┌─────▼─────┐              │
│              │  Commands │              │
│              └─────┬─────┘              │
│         ┌──────────┴──────────┐         │
│   ┌─────▼─────┐        ┌──────▼──────┐  │
│   │  DistFS   │        │   World     │  │
│   │(blockchain)│        │(peer mgmt)  │  │
│   └─────┬─────┘        └──────┬──────┘  │
│         └──────────┬──────────┘         │
│              ┌─────▼─────┐              │
│              │  Payload  │              │
│              └─────┬─────┘              │
│              ┌─────▼─────┐              │
│              │  Packet   │              │
│              └─────┬─────┘              │
│              ┌─────▼─────┐              │
│              │  Network  │              │
│              │   (UDP)   │              │
│              └───────────┘              │
└─────────────────────────────────────────┘
```

## Building

### Dependencies

- CMake >= 2.8.9
- OpenSSL (libssl-dev)
- libev (libev-dev)
- json-c (libjson-c-dev)
- readline (libreadline-dev)
- CUnit (libcunit1-dev) - for tests

### Ubuntu/Debian

```bash
sudo apt-get install build-essential cmake libssl-dev libev-dev \
                     libjson-c-dev libreadline-dev libcunit1-dev

git clone https://github.com/farb3yonddriv3n/skypeernet.git
cd skypeernet
mkdir build && cd build
cmake ..
make
sudo make install
```

### Docker

```bash
docker build -t skypeernet .
docker run -it skypeernet
```

## Quick Start

### 1. Start the Tracker (on a public server)

```bash
# Create configuration directory
mkdir -p /etc/skypeernet

# Copy and edit configuration
cp config/settings.cfg /etc/skypeernet/tracker.cfg

# Start tracker
spntracker
```

### 2. Start a Peer

```bash
# Edit peer configuration (set tracker_ip to your tracker's IP)
cp config/settings.cfg /etc/skypeernet/peer.cfg
# Edit tracker_ip in peer.cfg

# Start peer
spnpeer
```

### 3. Share a File

```bash
# Inside spnpeer CLI:

# 1. Put file in finalized directory
#    (copy your file to ~/finalized/myfile.txt)

# 2. Create a transaction to share it
ta myfile.txt "my tags here"

# 3. Mine the block to commit the transaction
bm

# 4. After mining completes, advertise your block to peers
ba a
```

### 4. Download a File

```bash
# Inside spnpeer CLI:

# 1. List remote files available
lf r

# 2. Find the file hash you want and add download job
ja <64-char-file-hash>

# 3. Check download progress
jd

# 4. When complete, finalize the file
jf <64-char-file-hash>

# File will be in ~/finalized/
```

## Configuration

Configuration file: `/etc/skypeernet/peer.cfg` (or `tracker.cfg`)

```json
{
    "tracker_ip"               : "127.0.0.1",    // Tracker IP address
    "tracker_port"             : 5775,           // Tracker UDP port
    "proxy"                    : 0,              // Act as proxy (0=no, 1=yes)

    "download_directory"       : "downloads",    // Temp download chunks
    "block_directory"          : "block",        // Blockchain storage
    "keys_directory"           : "keys",         // RSA keys
    "finalized_directory"      : "finalized",    // Completed files
    "tcp_directory"            : "tcp",          // TCP tunnel temp files

    "interval_retry"           : 0.050,          // Packet retry interval (sec)
    "interval_peers_reachable" : 5,              // Peer check interval (sec)

    "mining_target"            : 2,              // Mining difficulty (leading zeros)
    "send_factor"              : 2,              // Send rate multiplier

    "max_task_buffer"          : 51200,          // Max task buffer size
    "max_send_queue"           : 50,             // Max packets in send queue
    "max_send_retry"           : 200,            // Max retry attempts
    "max_peer_unreachable"     : 5,              // Max unreachable count before removal
    "max_chunk_size"           : 524288000,      // Max chunk size (500MB)

    "max_upload_kbytes"        : 2048,           // Upload limit (KB/s)
    "max_download_kbytes"      : 4096,           // Download limit (KB/s)

    "pipe_read"                : "/tmp/skypeernet_read",   // API read pipe
    "pipe_write"               : "/tmp/skypeernet_write",  // API write pipe

    "tcp_allowed_ports"        : [22, 80, 443],  // Exposed TCP ports
    "tcp_ports_description"    : "SSH, HTTP"     // Description for peers
}
```

## CLI Commands Reference

### Network & Peers

| Command | Alias | Arguments | Description |
|---------|-------|-----------|-------------|
| `peers` | `p`, `l`, `list` | - | List all connected peers |
| `whoami` | `w` | - | Show your IP, port, and public key hash |
| `msg` | `m` | `<host> <port> <message>` | Send message to peer |
| `filesend` | `fs` | `<host> <port> <filename>` | Send file to peer |
| `traffic` | `tf` | - | Show upload/download statistics |

### Whitelist (Tracker only)

| Command | Alias | Arguments | Description |
|---------|-------|-----------|-------------|
| `whitelist` | `wl` | - | List whitelisted public key hashes |
| `whitelist_add` | `wa` | `<pubkeyhash>` | Add peer to whitelist |
| `whitelist_rem` | `wr` | `<pubkeyhash>` | Remove peer from whitelist |

### File Sharing (Transactions & Blocks)

| Command | Alias | Arguments | Description |
|---------|-------|-----------|-------------|
| `tadd` | `ta` | `<filename> <tags>` | Create transaction to share a file |
| `tshare` | `ts` | `<filehash>` | Re-share existing file from network |
| `tlist` | `tl` | - | List pending transactions |
| `bmine` | `bm` | - | Mine a block with pending transactions |
| `blocksaction` | `ba` | `u` or `a` | Update (`u`) or Advertise (`a`) block |

### File Download (Jobs)

| Command | Alias | Arguments | Description |
|---------|-------|-----------|-------------|
| `listfiles` | `lf` | `l` or `r` | List local (`l`) or remote (`r`) files |
| `jobadd` | `ja` | `<filehash>` | Start downloading a file |
| `jobdump` | `jd` | - | Show download job status |
| `jobfinalize` | `jf` | `<filehash>` | Finalize completed download |
| `jobremove` | `jr` | `<filehash>` | Remove/cancel a download job |
| `jobclean` | `jc` | - | Clean up completed jobs |

### Tasks & Debug

| Command | Alias | Arguments | Description |
|---------|-------|-----------|-------------|
| `taskdump` | `ad` | - | Show active file transfer tasks |
| `taskcancel` | `ac` | `<taskindex>` | Cancel a transfer task |
| `keysdump` | `kd` | - | Show RSA key information |
| `rdump` | `rd` | - | Dump rogue peer info |
| `versiondump` | `v` | - | Show version information |

### TCP Tunneling

| Command | Alias | Arguments | Description |
|---------|-------|-----------|-------------|
| `tunneladd` | `ua` | `<pubkeyhash> <port>` | Open tunnel to peer's TCP port |
| `tunneldump` | `ud` | - | List active tunnels |
| `endpointdump` | `ed` | - | List tunnel endpoints |

## Encryption and Keys

### How Encryption Works

All files in skypeernet are **encrypted with AES-256**. The encryption key is derived from the sharer's RSA private key:

```
┌─────────────────────────────────────────────────────────────────┐
│  FILE ENCRYPTION (when sharing)                                  │
│                                                                   │
│  Your RSA Private Key                                            │
│         │                                                        │
│         ▼                                                        │
│  SHA256(private_key) ──► AES-256 Key                            │
│                              │                                   │
│  Original File ──────────────┼──► Encrypted Chunks              │
│                              │                                   │
│  Each chunk gets unique IV (timeiter)                           │
└─────────────────────────────────────────────────────────────────┘
```

### Key Requirement for Downloading

**Important:** To finalize (decrypt) a downloaded file, you MUST have the private key of the peer who originally shared it.

```
┌─────────────────────────────────────────────────────────────────┐
│  FILE DECRYPTION (when finalizing)                               │
│                                                                   │
│  Downloaded encrypted chunks                                     │
│         │                                                        │
│         │    Sharer's Private Key (from ~/keys/)                │
│         │              │                                         │
│         │              ▼                                         │
│         │    SHA256(private_key) ──► AES-256 Key                │
│         │              │                                         │
│         └──────────────┼──► Decrypted File                      │
│                                                                   │
│  ⚠️  WITHOUT the key: finalization FAILS silently               │
└─────────────────────────────────────────────────────────────────┘
```

### Managing Shared Keys

Keys are stored in the `keys_directory` (default: `~/keys/`):

```bash
~/keys/
├── private.pem          # Your private key (auto-generated)
├── public.pem           # Your public key (auto-generated)
└── <pubkeyhash>.pem     # Shared keys from other peers (64-char hex names)
```

**To download files from another peer:**

1. Get their private key file (they must share it with you out-of-band)
2. Rename it to their `pubkeyhash` (64 hex characters) with `.pem` extension
3. Place it in your `~/keys/` directory
4. Restart spnpeer (keys are loaded at startup)

**Check loaded keys:**
```bash
> kd
 | My key |
 | a1b2c3d4e5f6...your_pubkeyhash...64chars |
 | Shared keys |
 | f6e5d4c3b2a1...their_pubkeyhash...64chars |
```

**Finding who shared a file:**
```bash
> lf r
{
  "files": [{
    "name": "document.pdf",
    "hash": "...",
    "pubkeyhash": "f6e5d4c3b2a1..."   # ◄── You need this peer's private key
  }]
}
```

### Use Cases

| Scenario | Key Required | How to Get It |
|----------|--------------|---------------|
| Download your own files | Your private key | Already have it (auto) |
| Download from trusted peer | Their private key | They share it with you |
| Download from public peer | Their private key | They publish it |
| Public file sharing | Shared group key | Distributed to group |

### Security Implications

- **Private sharing:** Only share your private key with trusted parties
- **Public sharing:** If you want files publicly downloadable, publish your private key
- **Group sharing:** Create a dedicated keypair for the group and distribute it

## File Sharing Workflow

### Sharing a File (Step by Step)

```
┌──────────────────────────────────────────────────────────────────┐
│  YOUR MACHINE                                                     │
│                                                                   │
│  1. Place file in ~/finalized/                                   │
│     $ cp myfile.pdf ~/finalized/                                 │
│                                                                   │
│  2. Create transaction (in spnpeer CLI)                          │
│     > ta myfile.pdf "pdf documents work"                         │
│     Transaction added                                            │
│                                                                   │
│  3. Mine the block                                               │
│     > bm                                                         │
│     Mining finished. It took 2.345 seconds                       │
│                                                                   │
│  4. Advertise to network                                         │
│     > ba a                                                       │
│     Block action 2 to 5 peers                                    │
│                                                                   │
│  ✓ File is now visible to other peers via "lf r"                 │
└──────────────────────────────────────────────────────────────────┘
```

### Downloading a File (Step by Step)

```
┌──────────────────────────────────────────────────────────────────┐
│  YOUR MACHINE                                                     │
│                                                                   │
│  1. List available remote files                                  │
│     > lf r                                                       │
│     {                                                            │
│       "files": [{                                                │
│         "name": "myfile.pdf",                                    │
│         "hash": "a1b2c3d4...64chars...",                        │
│         "pubkeyhash": "f6e5d4c3...64chars...",  ◄── NOTE THIS!  │
│         "size": 1048576,                                         │
│         "downloaded": false                                      │
│       }]                                                         │
│     }                                                            │
│                                                                   │
│  ⚠️  BEFORE DOWNLOADING: Ensure you have the sharer's private   │
│     key in ~/keys/<pubkeyhash>.pem (see "Encryption and Keys")  │
│                                                                   │
│  2. Start download job (use the 64-char hash)                    │
│     > ja a1b2c3d4e5f6...                                        │
│     Job a1b2c3d4e5f6... added                                   │
│                                                                   │
│  3. Monitor progress                                             │
│     > jd                                                         │
│     {                                                            │
│       "jobs": [{                                                 │
│         "file": "a1b2c3d4...",                                  │
│         "progress": "75%",                                       │
│         "chunks_done": 3,                                        │
│         "chunks_total": 4                                        │
│       }]                                                         │
│     }                                                            │
│                                                                   │
│  4. Finalize when complete (100%)                                │
│     > jf a1b2c3d4e5f6...                                        │
│                                                                   │
│     ⚠️  If finalization fails silently, you're missing the key! │
│                                                                   │
│  ✓ File is now in ~/finalized/myfile.pdf                        │
└──────────────────────────────────────────────────────────────────┘
```

### Understanding File Hashes

Files in skypeernet are identified by SHA256 hashes (64 hexadecimal characters):

```
File: myfile.pdf
Hash: a1b2c3d4e5f678901234567890abcdef1234567890abcdef1234567890abcdef

This hash is used to:
- Uniquely identify the file across the network
- Verify file integrity after download
- Reference files in commands (ja, jf, jr, ts)
```

## TCP Tunneling

TCP tunneling allows you to access TCP services on remote peers through the P2P network.

### Example: Access SSH on a remote peer

```bash
# On your machine (inside spnpeer CLI):

# 1. Get the peer's public key hash
> p
# Find the peer and note their pubkeyhash

# 2. Open tunnel to their SSH port (22)
> ua a1b2c3d4e5f6...pubkeyhash...64chars 22
{
  "success": true,
  "src_port": 12345,    # Your local port
  "dst_port": 22         # Remote SSH port
}

# 3. Now connect via the tunnel
$ ssh user@localhost -p 12345
# This connects to the remote peer's SSH through the P2P network!

# 4. Check tunnel status
> ud

# 5. Close tunnel when done (not shown - tunnels persist until peer restart)
```

## Plugins

### johnconnor (Erlang/OTP)

Web-based management interface accessible at `http://localhost:8080`

```bash
cd plugins/johnconnor
./rebar get-deps
./rebar compile
./rel/files/erws start
```

### skynet (Python)

Automated file synchronization daemon.

```bash
cd plugins/skynet
pip install -r requirements.txt
python skynet.py
```

## Troubleshooting

### Peer won't connect to tracker

1. Check tracker IP in configuration
2. Ensure UDP port 5775 is open
3. Verify tracker is running: `netstat -ulnp | grep 5775`

### Files not downloading

1. Check peer connectivity: `p` (peers command)
2. Verify file exists remotely: `lf r`
3. Check job status: `jd`
4. Look at tasks: `ad`

### File finalization fails (no file appears)

**Most common cause:** Missing decryption key!

1. Check the file's `pubkeyhash` with `lf r`
2. Verify you have the key: `kd` (should show matching shared key)
3. If missing, obtain the private key from the file sharer
4. Place key in `~/keys/<pubkeyhash>.pem`
5. **Restart spnpeer** (keys only load at startup)
6. Try `jf <hash>` again

### Mining takes too long

Reduce `mining_target` in configuration (fewer leading zeros = easier mining)

### NAT/Firewall issues

1. Set `proxy: 1` on public-facing peers
2. Use TCP tunneling for direct connections
3. Check if peer shows `shadow: true` (being routed through proxy)

### Debug logging

Check syslog for detailed logs:
```bash
tail -f /var/log/syslog | grep -E "(distfs|spnpeer)"
```

## Protocol Details

See the header files in `src/include/` for detailed protocol documentation:
- `packet.h` - Packet structure and fragmentation
- `payload.h` - Command handlers
- `data.h` - Serialization format
- `peer.h` - Peer state machine
- `net.h` - Network layer
- `world.h` - Peer discovery

## License

GPL 3.0 - See [LICENSE](LICENSE) file.

## Contributing

Pull requests welcome! Please ensure:
1. Code follows existing style
2. Tests pass: `./spntest`
3. No compiler warnings with `-Wall`
