#!/usr/bin/env python3
"""
skypeernet desktop GUI.

A cross-platform (PyQt5) front-end for a running spnpeer daemon. It speaks the
peer's named-pipe API (see spnclient.py) and provides:

  * Share    - pick a file, choose plain or encrypted (optionally to specific
               peers), and publish it (auto mine + advertise).
  * Files    - browse files advertised by the network, search, and download;
               encrypted files are decrypted automatically on finalize.
  * Messages - per-peer chat.
  * Activity - live transfer/job state and an event log.

Usage:
    python3 app.py --peer-dir /path/to/spnpeer/workdir

The peer directory is where spnpeer runs (it contains config/settings.cfg).
The GUI reads that config for the pipe paths and the finalized directory.
"""

import sys
import os
import json
import html
import shutil
import argparse

# Qt binding compatibility: prefer PyQt5, fall back to PySide6.
try:
    from PyQt5 import QtCore, QtWidgets
    Signal = QtCore.pyqtSignal
    def _exec(a): return a.exec_()
except ImportError:  # pragma: no cover
    from PySide6 import QtCore, QtWidgets
    Signal = QtCore.Signal
    def _exec(a): return a.exec()

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from spnclient import (SpnClient, API_LISTPEERS, API_MESSAGE, API_LISTFILES_REMOTE,
                       API_JOBSDUMP, API_JOBDONE, API_TADD, API_BMINE, API_BADVERTISE,
                       API_BMINING, API_PEER_ONLINE, API_PEER_OFFLINE,
                       API_TRACKER_GET, API_TRACKER_SET,
                       SCHEME_PLAIN, SCHEME_LEGACY, SCHEME_CEK)

SCHEME_LABEL = {SCHEME_PLAIN: "plain", SCHEME_LEGACY: "legacy", SCHEME_CEK: "encrypted"}


def load_peer_config(peer_dir):
    cfg_path = os.path.join(peer_dir, "config", "settings.cfg")
    with open(cfg_path) as f:
        cfg = json.load(f)
    finalized = os.path.join(peer_dir, cfg.get("finalized_directory", "finalized"))
    return {
        "pipe_read":  cfg.get("pipe_read",  "/tmp/skypeernet_read"),
        "pipe_write": cfg.get("pipe_write", "/tmp/skypeernet_write"),
        "finalized":  finalized,
        "tracker_ip":   cfg.get("tracker_ip", "127.0.0.1"),
        "tracker_port": cfg.get("tracker_port", 5775),
    }


def save_tracker_to_config(peer_dir, ip, port):
    """Persist tracker_ip/tracker_port into settings.cfg, keeping other keys."""
    cfg_path = os.path.join(peer_dir, "config", "settings.cfg")
    with open(cfg_path) as f:
        cfg = json.load(f)
    cfg["tracker_ip"] = ip
    cfg["tracker_port"] = int(port)
    with open(cfg_path, "w") as f:
        json.dump(cfg, f, indent=4)
        f.write("\n")


class Bridge(QtCore.QObject):
    """Marshals reader-thread messages onto the Qt event loop."""
    received = Signal(object)
    errored = Signal(str)


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self, peer_dir, cfg):
        super().__init__()
        self.peer_dir = peer_dir
        self.cfg = cfg
        self.setWindowTitle("skypeernet")
        self.resize(1000, 660)

        self.peers = {}          # pubkeyhash -> peer dict
        self.share_queue = []    # filenames awaiting mine->advertise
        self.pending_jobs = set()  # file hashes we've asked to download
        self.chats = {}          # pubkeyhash -> list[(who, text)]
        self.tracker = (cfg.get("tracker_ip", "127.0.0.1"),
                        cfg.get("tracker_port", 5775))

        self.bridge = Bridge()
        self.bridge.received.connect(self.on_message)
        self.bridge.errored.connect(lambda m: self.log("! " + m))
        self.client = SpnClient(cfg["pipe_read"], cfg["pipe_write"],
                                on_message=self.bridge.received.emit,
                                on_error=self.bridge.errored.emit)

        self._build_ui()
        self.client.start()

        self.poll = QtCore.QTimer(self)
        self.poll.timeout.connect(self._refresh)
        self.poll.start(3000)
        QtCore.QTimer.singleShot(400, self._refresh)

    # ---- UI construction -------------------------------------------

    def _build_ui(self):
        central = QtWidgets.QWidget()
        self.setCentralWidget(central)
        outer = QtWidgets.QVBoxLayout(central)

        statusrow = QtWidgets.QHBoxLayout()
        self.status = QtWidgets.QLabel()
        self.status.setStyleSheet("padding:4px;")
        statusrow.addWidget(self.status, 1)
        self.tracker_btn = QtWidgets.QPushButton("Tracker…")
        self.tracker_btn.setToolTip("View or change the tracker address")
        self.tracker_btn.clicked.connect(self._edit_tracker)
        statusrow.addWidget(self.tracker_btn)
        outer.addLayout(statusrow)

        tabs = QtWidgets.QTabWidget()
        outer.addWidget(tabs, 1)
        tabs.addTab(self._build_files_tab(), "Files")
        tabs.addTab(self._build_share_tab(), "Share")
        tabs.addTab(self._build_messages_tab(), "Messages")
        tabs.addTab(self._build_activity_tab(), "Activity")
        self._update_status()

    def _build_files_tab(self):
        w = QtWidgets.QWidget()
        v = QtWidgets.QVBoxLayout(w)
        bar = QtWidgets.QHBoxLayout()
        bar.addWidget(QtWidgets.QLabel("Search:"))
        self.search = QtWidgets.QLineEdit()
        self.search.setPlaceholderText("filter by name, tag, owner or hash")
        self.search.textChanged.connect(self._render_files)
        bar.addWidget(self.search, 1)
        refresh = QtWidgets.QPushButton("Refresh")
        refresh.clicked.connect(lambda: self.client.list_files_remote())
        bar.addWidget(refresh)
        v.addLayout(bar)

        self.files_table = QtWidgets.QTreeWidget()
        self.files_table.setHeaderLabels(
            ["Name", "Type", "Owner", "Size", "Progress", "Status"])
        self.files_table.setRootIsDecorated(False)
        self.files_table.setAlternatingRowColors(True)
        self.files_table.setColumnWidth(0, 300)
        self.files_table.itemSelectionChanged.connect(self._files_selection_changed)
        v.addWidget(self.files_table, 1)

        row = QtWidgets.QHBoxLayout()
        self.download_btn = QtWidgets.QPushButton("Download / Decrypt")
        self.download_btn.setEnabled(False)
        self.download_btn.clicked.connect(self._download_selected)
        row.addStretch(1)
        row.addWidget(self.download_btn)
        v.addLayout(row)

        self._files_cache = []   # list of file dicts
        return w

    def _build_share_tab(self):
        w = QtWidgets.QWidget()
        form = QtWidgets.QFormLayout(w)

        pick = QtWidgets.QHBoxLayout()
        self.share_path = QtWidgets.QLineEdit()
        self.share_path.setReadOnly(True)
        browse = QtWidgets.QPushButton("Choose file…")
        browse.clicked.connect(self._pick_file)
        pick.addWidget(self.share_path, 1)
        pick.addWidget(browse)
        form.addRow("File:", self._wrap(pick))

        self.share_tags = QtWidgets.QLineEdit()
        self.share_tags.setPlaceholderText("space or comma separated tags")
        form.addRow("Tags:", self.share_tags)

        self.enc_group = QtWidgets.QButtonGroup(w)
        self.rb_plain = QtWidgets.QRadioButton("Plain (anyone can download)")
        self.rb_enc = QtWidgets.QRadioButton("Encrypted")
        self.rb_enc.setChecked(True)
        self.enc_group.addButton(self.rb_plain)
        self.enc_group.addButton(self.rb_enc)
        enc = QtWidgets.QVBoxLayout()
        enc.addWidget(self.rb_enc)
        enc.addWidget(self.rb_plain)
        form.addRow("Encryption:", self._wrap(enc))

        self.share_to = QtWidgets.QLineEdit()
        self.share_to.setPlaceholderText(
            "optional: recipient pubkeyhashes, comma separated (blank = only you)")
        form.addRow("Grant to:", self.share_to)
        self.rb_enc.toggled.connect(lambda on: self.share_to.setEnabled(on))

        self.share_btn = QtWidgets.QPushButton("Share")
        self.share_btn.clicked.connect(self._do_share)
        form.addRow("", self.share_btn)

        note = QtWidgets.QLabel(
            "Sharing copies the file into the peer's finalized directory, "
            "creates a transaction, mines a block, and advertises it to peers.")
        note.setWordWrap(True)
        note.setStyleSheet("color:#666;")
        form.addRow("", note)
        return w

    def _build_messages_tab(self):
        w = QtWidgets.QWidget()
        h = QtWidgets.QHBoxLayout(w)

        left = QtWidgets.QVBoxLayout()
        left.addWidget(QtWidgets.QLabel("Peers"))
        self.peers_list = QtWidgets.QListWidget()
        self.peers_list.itemSelectionChanged.connect(self._render_chat)
        left.addWidget(self.peers_list, 1)
        h.addLayout(left, 1)

        right = QtWidgets.QVBoxLayout()
        self.chat_view = QtWidgets.QTextEdit()
        self.chat_view.setReadOnly(True)
        right.addWidget(self.chat_view, 1)
        entry = QtWidgets.QHBoxLayout()
        self.chat_input = QtWidgets.QLineEdit()
        self.chat_input.setPlaceholderText("type a message and press Enter")
        self.chat_input.returnPressed.connect(self._send_message)
        send = QtWidgets.QPushButton("Send")
        send.clicked.connect(self._send_message)
        entry.addWidget(self.chat_input, 1)
        entry.addWidget(send)
        right.addLayout(entry)
        h.addLayout(right, 2)
        return w

    def _build_activity_tab(self):
        w = QtWidgets.QWidget()
        v = QtWidgets.QVBoxLayout(w)
        v.addWidget(QtWidgets.QLabel("Transfers"))
        self.jobs_table = QtWidgets.QTreeWidget()
        self.jobs_table.setHeaderLabels(["File", "Chunks", "Receiving", "Done"])
        self.jobs_table.setRootIsDecorated(False)
        v.addWidget(self.jobs_table)
        v.addWidget(QtWidgets.QLabel("Log"))
        self.log_view = QtWidgets.QPlainTextEdit()
        self.log_view.setReadOnly(True)
        self.log_view.setMaximumBlockCount(500)
        v.addWidget(self.log_view, 1)
        return w

    @staticmethod
    def _wrap(layout):
        c = QtWidgets.QWidget()
        c.setLayout(layout)
        return c

    # ---- periodic refresh ------------------------------------------

    def _refresh(self):
        if not self.client.connected():
            self._update_status()
            return
        self.client.list_peers()
        self.client.list_files_remote()
        self.client.jobs_dump()
        self.client.tracker_get()
        self._update_status()

    def _update_status(self):
        up = self.client.connected()
        n = len([p for p in self.peers.values() if p.get("type") == 2])
        self.status.setText(
            ("● connected" if up else "○ waiting for peer…")
            + "   |   tracker: %s:%s   |   peers: %d"
            % (self.tracker[0], self.tracker[1], n))
        self.status.setStyleSheet(
            "padding:4px;color:%s;" % ("#166534" if up else "#9a3412"))

    # ---- message dispatch (Qt thread) ------------------------------

    def on_message(self, obj):
        cmd = obj.get("command")
        payload = obj.get("payload") or {}
        if cmd == API_LISTPEERS:
            self._update_peers(payload.get("peers", []))
        elif cmd == API_LISTFILES_REMOTE:
            self._update_files(payload)
        elif cmd == API_JOBSDUMP:
            self._update_jobs(payload.get("jobs", []))
        elif cmd == API_MESSAGE and "message" in payload:
            self._incoming_message(payload)
        elif cmd == API_JOBDONE:
            h = payload.get("name", "")
            self.log("job complete: %s" % h[:16])
            if h in self.pending_jobs:
                self.client.job_finalize(h)
        elif cmd == API_PEER_ONLINE:
            self.log("peer online")
            self.client.list_peers()
        elif cmd == API_PEER_OFFLINE:
            self.log("peer offline")
            self.client.list_peers()
        elif cmd == API_TADD:
            self._after_tadd(obj.get("error", 0))
        elif cmd == API_BMINE:
            if obj.get("error"):
                self.log("mine rejected (err %s)" % obj.get("error"))
        elif cmd == API_BMINING:
            self._mining_tick(payload.get("locked", False))
        elif cmd == API_TRACKER_GET:
            self.tracker = (payload.get("ip", self.tracker[0]),
                            payload.get("port", self.tracker[1]))
            self._update_status()
        elif cmd == API_TRACKER_SET:
            self._after_tracker_set(obj.get("error", 0))

    # ---- peers -----------------------------------------------------

    def _update_peers(self, peers):
        self.peers = {}
        for pr in peers:
            self.peers[pr.get("pubkeyhash", "")] = pr
        sel = self._selected_peer_hash()
        self.peers_list.blockSignals(True)
        self.peers_list.clear()
        for h, pr in self.peers.items():
            if pr.get("type") != 2:
                continue
            label = h[:16] + "…"
            if pr.get("tcpdesc"):
                label += "  (%s)" % pr["tcpdesc"]
            if pr.get("offline"):
                label += "  [offline]"
            it = QtWidgets.QListWidgetItem(label)
            it.setData(QtCore.Qt.UserRole, h)
            self.peers_list.addItem(it)
            if h == sel:
                it.setSelected(True)
        self.peers_list.blockSignals(False)
        self._update_status()

    def _selected_peer_hash(self):
        it = self.peers_list.currentItem()
        return it.data(QtCore.Qt.UserRole) if it else None

    # ---- files -----------------------------------------------------

    def _update_files(self, payload):
        files = []
        roots = list(payload.get("roots", []))
        localwrap = payload.get("local")
        if localwrap:
            roots += localwrap.get("roots", [])
        for root in roots:
            owner = root.get("owner", "")
            for blk in root.get("blocks", []):
                for t in blk.get("transactions", []):
                    t = dict(t)
                    t["_owner"] = owner
                    files.append(t)
        self._files_cache = files
        self._render_files()

    def _render_files(self):
        q = self.search.text().strip().lower()
        keep = self._selected_file_hash()
        self.files_table.clear()
        for t in self._files_cache:
            name = t.get("description") or ("(encrypted: " + t.get("name", "")[:16] + "…)")
            owner = t.get("_owner", "")
            hay = " ".join([str(name), t.get("tags", ""), owner, t.get("name", "")]).lower()
            if q and q not in hay:
                continue
            scheme = t.get("scheme", SCHEME_PLAIN)
            typ = SCHEME_LABEL.get(scheme, "?")
            if scheme != SCHEME_PLAIN and not t.get("decryptable", False):
                typ += " (no key)"
            size = self._human(t.get("size", 0))
            prog = "%s/%s" % (t.get("chunks_done", 0), t.get("chunks_total", 0))
            if t.get("finalized"):
                st = "finalized"
            elif t.get("complete"):
                st = "downloaded"
            elif t.get("name") in self.pending_jobs:
                st = "downloading"
            else:
                st = "available"
            it = QtWidgets.QTreeWidgetItem([name, typ, owner[:12] + "…", size, prog, st])
            it.setData(0, QtCore.Qt.UserRole, t)
            if scheme != SCHEME_PLAIN:
                it.setIcon(1, self.style().standardIcon(
                    QtWidgets.QStyle.SP_DialogNoButton if not t.get("decryptable")
                    else QtWidgets.QStyle.SP_DialogYesButton))
            self.files_table.addTopLevelItem(it)
            if t.get("name") == keep:
                it.setSelected(True)
        self._files_selection_changed()

    def _selected_file(self):
        it = self.files_table.currentItem()
        return it.data(0, QtCore.Qt.UserRole) if it else None

    def _selected_file_hash(self):
        t = self._selected_file()
        return t.get("name") if t else None

    def _files_selection_changed(self):
        t = self._selected_file()
        can = False
        if t:
            undownloadable = (t.get("scheme") != SCHEME_PLAIN
                              and not t.get("decryptable", False))
            can = not t.get("finalized") and not undownloadable
        self.download_btn.setEnabled(can)

    def _download_selected(self):
        t = self._selected_file()
        if not t:
            return
        h = t.get("name")
        self.pending_jobs.add(h)
        self.client.job_add(h)
        self.log("download requested: %s" % (t.get("description") or h[:16]))
        self._render_files()

    # ---- share -----------------------------------------------------

    def _pick_file(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Choose file to share")
        if path:
            self.share_path.setText(path)

    def _do_share(self):
        path = self.share_path.text().strip()
        if not path or not os.path.isfile(path):
            QtWidgets.QMessageBox.warning(self, "Share", "Choose a valid file first.")
            return
        os.makedirs(self.cfg["finalized"], exist_ok=True)
        name = os.path.basename(path)
        dst = os.path.join(self.cfg["finalized"], name)
        try:
            if os.path.abspath(path) != os.path.abspath(dst):
                shutil.copy2(path, dst)
        except OSError as e:
            QtWidgets.QMessageBox.critical(self, "Share", "Copy failed: %s" % e)
            return
        encrypted = self.rb_enc.isChecked()
        to = self.share_to.text().strip() if encrypted else ""
        ok = self.client.share(name, tags=self.share_tags.text().strip(),
                               encrypted=encrypted, to=to or None)
        if not ok:
            QtWidgets.QMessageBox.critical(self, "Share", "Peer not reachable.")
            return
        self.share_queue.append(name)
        self.share_btn.setEnabled(False)
        self.log("sharing %s (%s)…" % (name, "encrypted" if encrypted else "plain"))

    def _after_tadd(self, error):
        if error:
            self.share_btn.setEnabled(True)
            QtWidgets.QMessageBox.critical(
                self, "Share",
                "Peer rejected the file (error %s). If you targeted recipients, "
                "they must be online or their public key imported." % error)
            self.log("share failed (err %s)" % error)
            return
        self.log("transaction added, mining…")
        self.client.mine()
        self._mining_wait = True
        QtCore.QTimer.singleShot(500, self.client.mining_state)

    def _mining_tick(self, locked):
        if not getattr(self, "_mining_wait", False):
            return
        if locked:
            QtCore.QTimer.singleShot(600, self.client.mining_state)
        else:
            self._mining_wait = False
            self.client.advertise()
            self.log("mined + advertised to peers")
            self.share_btn.setEnabled(True)
            self.client.list_files_remote()

    # ---- jobs ------------------------------------------------------

    def _update_jobs(self, jobs):
        self.jobs_table.clear()
        for j in jobs:
            name = j.get("name", "")[:16] + "…"
            it = QtWidgets.QTreeWidgetItem([
                name, str(j.get("chunks_size", 0)),
                str(j.get("counter_receiving", 0)),
                str(j.get("counter_done", 0))])
            self.jobs_table.addTopLevelItem(it)

    # ---- messages --------------------------------------------------

    def _render_chat(self):
        h = self._selected_peer_hash()
        self.chat_view.clear()
        for who, text in self.chats.get(h, []):
            color = "#166534" if who == "me" else "#1e3a8a"
            self.chat_view.append(
                '<span style="color:%s"><b>%s:</b></span> %s'
                % (color, "you" if who == "me" else "peer", html.escape(text)))

    def _send_message(self):
        h = self._selected_peer_hash()
        text = self.chat_input.text().strip()
        if not h or not text:
            return
        pr = self.peers.get(h)
        if not pr:
            return
        host_dec = int(pr["host"], 16)
        if self.client.message(host_dec, pr["port"], text):
            self.chats.setdefault(h, []).append(("me", text))
            self.chat_input.clear()
            self._render_chat()
        else:
            self.log("message send failed")

    def _incoming_message(self, payload):
        host_hex = payload.get("host", "")
        port = payload.get("port")
        text = payload.get("message", "")
        # match sender to a known peer by host+port
        sender = None
        for h, pr in self.peers.items():
            if pr.get("host") == host_hex and pr.get("port") == port:
                sender = h
                break
        key = sender or ("%s:%s" % (host_hex, port))
        self.chats.setdefault(key, []).append(("peer", text))
        self.log("message from %s: %s" % (key[:12], text))
        if key == self._selected_peer_hash():
            self._render_chat()

    # ---- tracker ---------------------------------------------------

    def _edit_tracker(self):
        dlg = QtWidgets.QDialog(self)
        dlg.setWindowTitle("Tracker address")
        form = QtWidgets.QFormLayout(dlg)
        ip_edit = QtWidgets.QLineEdit(str(self.tracker[0]))
        port_edit = QtWidgets.QSpinBox()
        port_edit.setRange(1, 65535)
        try:
            port_edit.setValue(int(self.tracker[1]))
        except (TypeError, ValueError):
            port_edit.setValue(5775)
        form.addRow("Tracker IP:", ip_edit)
        form.addRow("Tracker port:", port_edit)
        persist = QtWidgets.QCheckBox("Also save to settings.cfg (used on restart)")
        persist.setChecked(True)
        form.addRow("", persist)
        note = QtWidgets.QLabel(
            "Applying re-announces this peer to the new tracker immediately.")
        note.setWordWrap(True)
        note.setStyleSheet("color:#666;")
        form.addRow("", note)
        buttons = QtWidgets.QDialogButtonBox(
            QtWidgets.QDialogButtonBox.Ok | QtWidgets.QDialogButtonBox.Cancel)
        buttons.accepted.connect(dlg.accept)
        buttons.rejected.connect(dlg.reject)
        form.addRow(buttons)

        if dlg.exec_() != QtWidgets.QDialog.Accepted:
            return
        ip = ip_edit.text().strip()
        port = port_edit.value()
        if not self._valid_ipv4(ip):
            QtWidgets.QMessageBox.warning(
                self, "Tracker", "Enter a valid IPv4 address (e.g. 203.0.113.7).")
            return
        if not self.client.tracker_set(ip, port):
            QtWidgets.QMessageBox.critical(self, "Tracker", "Peer not reachable.")
            return
        self.tracker = (ip, port)
        self._update_status()
        if persist.isChecked():
            try:
                save_tracker_to_config(self.peer_dir, ip, port)
                self.log("tracker saved to settings.cfg")
            except Exception as e:
                QtWidgets.QMessageBox.warning(
                    self, "Tracker",
                    "Applied live, but saving to settings.cfg failed:\n%s" % e)
        self.log("tracker set to %s:%s (re-announcing)" % (ip, port))

    def _after_tracker_set(self, error):
        if error:
            msg = {1: "invalid address", 2: "announce failed"}.get(
                error, "error %s" % error)
            self.log("tracker change rejected: %s" % msg)
            QtWidgets.QMessageBox.critical(
                self, "Tracker", "Peer rejected the tracker change (%s)." % msg)
        else:
            self.client.list_peers()

    @staticmethod
    def _valid_ipv4(ip):
        parts = ip.split(".")
        if len(parts) != 4:
            return False
        try:
            return all(0 <= int(p) <= 255 for p in parts)
        except ValueError:
            return False

    # ---- misc ------------------------------------------------------

    @staticmethod
    def _human(n):
        try:
            n = float(n)
        except (TypeError, ValueError):
            return str(n)
        for unit in ["B", "KB", "MB", "GB", "TB"]:
            if n < 1024:
                return "%.0f %s" % (n, unit) if unit == "B" else "%.1f %s" % (n, unit)
            n /= 1024
        return "%.1f PB" % n

    def log(self, text):
        ts = QtCore.QTime.currentTime().toString("HH:mm:ss")
        self.log_view.appendPlainText("[%s] %s" % (ts, text))

    def closeEvent(self, ev):
        self.client.stop()
        super().closeEvent(ev)


def has_peer_config(peer_dir):
    return bool(peer_dir) and os.path.isfile(
        os.path.join(peer_dir, "config", "settings.cfg"))


def candidate_peer_dirs(explicit):
    """Ordered guesses for where spnpeer runs (contains config/settings.cfg)."""
    seen, out = set(), []
    def add(d):
        if not d:
            return
        d = os.path.abspath(d)
        if d not in seen:
            seen.add(d); out.append(d)
    add(explicit)
    add(os.getcwd())
    # repo root relative to this file (gui/.. ), and its parent
    here = os.path.dirname(os.path.abspath(__file__))
    add(os.path.dirname(here))
    add(os.path.expanduser("~/skypeernet"))
    add(os.path.expanduser("~/.skypeernet"))
    return out


def resolve_peer_dir(app, explicit):
    for d in candidate_peer_dirs(explicit):
        if has_peer_config(d):
            return d
    # nothing auto-detected — ask the user, don't die
    start = explicit or os.getcwd()
    while True:
        QtWidgets.QMessageBox.information(
            None, "skypeernet",
            "Select the folder where spnpeer runs.\n\n"
            "It is the working directory that contains config/settings.cfg "
            "(the same folder you start the peer from).")
        d = QtWidgets.QFileDialog.getExistingDirectory(
            None, "Select spnpeer working directory", start)
        if not d:
            return None  # user cancelled
        if has_peer_config(d):
            return d
        QtWidgets.QMessageBox.warning(
            None, "skypeernet",
            "No config/settings.cfg in:\n%s\n\nPick the peer's working "
            "directory (the one containing a 'config' folder)." % d)
        start = d


def main():
    ap = argparse.ArgumentParser(description="skypeernet desktop GUI")
    ap.add_argument("--peer-dir", default=None,
                    help="directory where spnpeer runs (contains config/settings.cfg); "
                         "if omitted, common locations are tried and then a folder "
                         "picker is shown")
    args = ap.parse_args()

    app = QtWidgets.QApplication(sys.argv)
    app.setApplicationName("skypeernet")

    peer_dir = resolve_peer_dir(app, args.peer_dir)
    if not peer_dir:
        return
    try:
        cfg = load_peer_config(peer_dir)
    except Exception as e:
        QtWidgets.QMessageBox.critical(
            None, "skypeernet",
            "Cannot read peer config in:\n%s\n\n%s" % (peer_dir, e))
        return

    win = MainWindow(peer_dir, cfg)
    win.show()
    sys.exit(_exec(app))


if __name__ == "__main__":
    main()
