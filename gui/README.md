# skypeernet GUI

A cross-platform desktop front-end (Python + Qt) for a running `spnpeer`
daemon. It drives the peer through its named-pipe API — the same interface the
`skynet` plugin uses — so the daemon does all the crypto and networking; the
GUI is a thin, safe client.

## Features

- **Share** a file as **plain** (anyone can download) or **encrypted**
  (only you, or specific peers you grant). Sharing automatically copies the
  file into the peer's finalized directory, creates the transaction, mines the
  block, and advertises it to the network.
- **Files** — browse everything the network has advertised, with a **search**
  box (name / tag / owner / hash). Each file shows whether it is plain or
  encrypted, and — for encrypted files — whether you hold a key for it.
  One click **downloads and decrypts** (decryption happens automatically once
  all chunks arrive).
- **Messages** — per-peer chat.
- **Activity** — live transfer progress and an event log.
- **Tracker** — the status bar shows the current tracker and a **Tracker…**
  button lets you change its address. Applying re-announces the peer to the
  new tracker immediately (no restart) and, by default, also saves it to
  `settings.cfg` so it persists.

## Requirements

- A built `spnpeer` (see the top-level README).
- Python 3.7+
- A Qt binding: **PyQt5** (recommended, widely packaged) or **PySide6**.
  The app auto-detects whichever is installed.

Install a binding:

```bash
pip install -r requirements.txt        # PyQt5
# or
pip install PySide6
# or via your OS package manager, e.g. Debian/Ubuntu:
sudo apt-get install python3-pyqt5
```

## Running

Start your peer as usual (from its working directory, which contains
`config/settings.cfg`):

```bash
cd /path/to/peer && spnpeer
```

Then launch the GUI:

```bash
python3 gui/app.py                      # auto-detects the peer directory
python3 gui/app.py --peer-dir /path/to/peer   # or point it explicitly
```

Without `--peer-dir`, the GUI looks for `config/settings.cfg` in the current
directory, the repo root, and `~/skypeernet` / `~/.skypeernet`. If none is
found it shows a folder picker — select the directory you start the peer from
(the one containing a `config` folder).

The GUI reads `config/settings.cfg` to find the peer's API pipes
(`pipe_read` / `pipe_write`) and its finalized directory. It never needs the
peer's keys — grants and decryption are performed by the daemon.

## Trying two clients on one machine

Each GUI attaches to its own `spnpeer`, so "two clients that see each other"
means one tracker plus two peers, each with its own working directory and
each whitelisted on the tracker. The helper script sets all of that up:

```bash
tools/localnet/localnet.sh up 2      # tracker + peerA + peerB
```

It prints a `--peer-dir` launch command for each peer; run each in its own
terminal:

```bash
python3 gui/app.py --peer-dir localnet/peerA
python3 gui/app.py --peer-dir localnet/peerB
```

Both windows will list each other under **Peers / Messages** within a few
seconds. Share a file from one and it appears in the other's **Files** tab.
Stop everything with `tools/localnet/localnet.sh down`. (Set `SPN_BASE` to
change where the working directories are created, `SPN_BIN` if the binaries
aren't in `build/`.)

## Sharing to specific peers

Choose **Encrypted** and put one or more recipient public-key hashes (comma
separated) in **Grant to**. Recipients are resolved from the live peer list or
from public keys you've imported (`keys/<pubkeyhash>.pub.pem`). Leave it blank
to keep the file private to yourself; you can grant access later from the CLI
with `ks <filehash> <pubkeyhash>`. Group keys (`kg` / `.grp`) also work as a
recipient hash. See the main README's "Encryption and Keys" section.

## Platform notes

The peer exposes its API over POSIX named pipes (`mkfifo`), so this transport
works on **Linux and macOS**. On **Windows**, run the peer under WSL and point
the GUI at the WSL-side pipe paths; the GUI code itself (Qt + `spnclient.py`)
is otherwise platform-neutral. A native Windows transport would require adding
a TCP control socket to the C daemon.

## Architecture

```
app.py         Qt UI. Marshals pipe-reader messages onto the Qt event loop
               via a QObject signal, so all widget access stays on the main
               thread.
spnclient.py   Transport + protocol. Background reader thread, thread-safe
               non-blocking writer. Framing (verified against the C peer):
                 GUI  -> peer : 4-byte BIG-endian length + JSON
                 peer -> GUI  : 4-byte LITTLE-endian length + JSON
```

`spnclient.py` has no Qt dependency and can be reused for headless scripting.
