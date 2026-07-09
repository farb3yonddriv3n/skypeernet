"""
skypeernet API client.

Talks to a running spnpeer over its two named pipes (the same interface the
skynet plugin uses). Framing, verified empirically against the C peer:

  GUI  -> peer  (pipe_read) : 4-byte BIG-endian length prefix + JSON
  peer -> GUI   (pipe_write): 4-byte LITTLE-endian length prefix + JSON

The client runs its reader in a background thread and dispatches decoded
messages to a single callback. All public send_* methods are thread-safe
(guarded by a write lock) and non-blocking.

This transport is POSIX (Linux/macOS) because spnpeer creates the pipes with
mkfifo. On Windows, run the peer under WSL and point the GUI at the WSL pipes,
or add a TCP transport to the C peer (out of scope here).
"""

import os
import json
import struct
import threading
import time

# API command ids — must match src/include/api.h
API_LISTPEERS        = 0
API_MESSAGE          = 1
API_LISTFILES_LOCAL  = 2
API_LISTFILES_REMOTE = 3
API_PEER_ONLINE      = 4
API_PEER_OFFLINE     = 5
API_JOBSDUMP         = 6
API_JOBDONE          = 7
API_JOBADD           = 8
API_JOBFINALIZE      = 9
API_TSHARE           = 10
API_BMINE            = 11
API_BADVERTISE       = 12
API_BMINING          = 13
API_ROGUEDUMP        = 14
API_VERSIONDUMP      = 15
API_TRAFFICDUMP      = 16
API_TASKDUMP         = 17
API_TADD             = 24
API_TRACKER_GET      = 25
API_TRACKER_SET      = 26

# file encryption schemes — must match src/include/file.h
SCHEME_PLAIN  = 0
SCHEME_LEGACY = 1
SCHEME_CEK    = 2


class SpnClient:
    def __init__(self, pipe_read, pipe_write, on_message, on_error=None):
        """
        pipe_read  : path the peer READS from (we write commands here)
        pipe_write : path the peer WRITES to  (we read replies here)
        on_message : callable(dict) invoked for every decoded message
        on_error   : optional callable(str)
        """
        self.pipe_read = pipe_read
        self.pipe_write = pipe_write
        self.on_message = on_message
        self.on_error = on_error or (lambda m: None)
        self._wfd = None
        self._wlock = threading.Lock()
        self._stop = threading.Event()
        self._rthread = None
        self._request_id = 0

    # ---- lifecycle -------------------------------------------------

    def start(self):
        self._stop.clear()
        self._rthread = threading.Thread(target=self._reader_loop, daemon=True)
        self._rthread.start()

    def stop(self):
        self._stop.set()
        with self._wlock:
            if self._wfd is not None:
                try:
                    os.close(self._wfd)
                except OSError:
                    pass
                self._wfd = None

    def connected(self):
        return os.path.exists(self.pipe_read) and os.path.exists(self.pipe_write)

    # ---- reading (peer -> GUI) -------------------------------------

    def _reader_loop(self):
        while not self._stop.is_set():
            try:
                fd = os.open(self.pipe_write, os.O_RDONLY | os.O_NONBLOCK)
            except OSError:
                time.sleep(0.5)
                continue
            buf = b""
            try:
                while not self._stop.is_set():
                    try:
                        chunk = os.read(fd, 65536)
                    except BlockingIOError:
                        time.sleep(0.03)
                        continue
                    except OSError:
                        break
                    if not chunk:
                        time.sleep(0.03)
                        continue
                    buf += chunk
                    buf = self._drain(buf)
            finally:
                try:
                    os.close(fd)
                except OSError:
                    pass
            time.sleep(0.2)

    def _drain(self, buf):
        while len(buf) >= 4:
            n = int.from_bytes(buf[:4], "little")
            if n <= 0 or n > 50_000_000:
                # desync — drop a byte and resync
                return buf[1:]
            if len(buf) < 4 + n:
                break
            body = buf[4:4 + n]
            buf = buf[4 + n:]
            try:
                obj = json.loads(body.decode("utf-8"))
            except Exception:
                continue
            try:
                self.on_message(obj)
            except Exception as e:
                self.on_error("dispatch: %s" % e)
        return buf

    # ---- writing (GUI -> peer) -------------------------------------

    def _send(self, cmd, extra=None):
        msg = dict(extra or {})
        msg["command"] = cmd
        msg["request_id"] = self._request_id
        self._request_id += 1
        body = json.dumps(msg).encode("utf-8")
        frame = struct.pack(">I", len(body)) + body
        with self._wlock:
            try:
                if self._wfd is None:
                    self._wfd = os.open(self.pipe_read, os.O_WRONLY | os.O_NONBLOCK)
                os.write(self._wfd, frame)
                return True
            except OSError as e:
                # peer not up yet / went away — reset and report
                if self._wfd is not None:
                    try:
                        os.close(self._wfd)
                    except OSError:
                        pass
                    self._wfd = None
                self.on_error("send: %s" % e)
                return False

    # ---- high-level commands ---------------------------------------

    def list_peers(self):        return self._send(API_LISTPEERS)
    def list_files_remote(self): return self._send(API_LISTFILES_REMOTE)
    def list_files_local(self):  return self._send(API_LISTFILES_LOCAL)
    def jobs_dump(self):         return self._send(API_JOBSDUMP)
    def mining_state(self):      return self._send(API_BMINING)
    def mine(self):              return self._send(API_BMINE)
    def advertise(self):         return self._send(API_BADVERTISE)
    def tracker_get(self):       return self._send(API_TRACKER_GET)

    def tracker_set(self, ip, port):
        return self._send(API_TRACKER_SET, {"ip": str(ip), "port": int(port)})

    def share(self, name, tags="", encrypted=True, to=None):
        """Share a file already placed in the peer's finalized directory."""
        extra = {"name": name, "tags": tags, "encrypted": bool(encrypted)}
        if to:
            extra["to"] = to if isinstance(to, str) else ",".join(to)
        return self._send(API_TADD, extra)

    def job_add(self, filehash):
        return self._send(API_JOBADD, {"name": filehash})

    def job_finalize(self, filehash):
        return self._send(API_JOBFINALIZE, {"name": filehash})

    def message(self, host, port, text):
        """host is the decimal integer form of the peer address."""
        return self._send(API_MESSAGE,
                          {"host": int(host), "port": int(port), "message": text})
