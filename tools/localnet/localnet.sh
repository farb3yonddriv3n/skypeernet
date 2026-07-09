#!/usr/bin/env bash
#
# localnet.sh — spin up a local skypeernet: one tracker + N peers on this
# machine, each with its own working directory, pipes, keys, and a shared
# tracker whitelist. Prints the GUI launch command for each peer.
#
# Usage:
#   tools/localnet/localnet.sh up [count]     # start tracker + <count> peers (default 2)
#   tools/localnet/localnet.sh down           # stop everything
#   tools/localnet/localnet.sh gui <name>     # launch the GUI for a peer (e.g. peerA)
#
# Config:
#   SPN_BASE  working root (default: ./localnet)
#   SPN_BIN   directory with spnpeer/spntracker (default: <repo>/build, then PATH)
#   SPN_PORT  tracker UDP port (default: 5775)
#
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
BASE="${SPN_BASE:-$repo/localnet}"
PORT="${SPN_PORT:-5775}"

find_bin() {
    if [ -n "${SPN_BIN:-}" ] && [ -x "$SPN_BIN/$1" ]; then echo "$SPN_BIN/$1"; return; fi
    if [ -x "$repo/build/$1" ]; then echo "$repo/build/$1"; return; fi
    if command -v "$1" >/dev/null 2>&1; then command -v "$1"; return; fi
    echo "ERROR: cannot find '$1' (build it, or set SPN_BIN)" >&2; exit 1
}
SPNPEER="$(find_bin spnpeer)"
SPNTRACKER="$(find_bin spntracker)"

# The binaries link against libskypeernettcp.so (built next to them). Make sure
# the dynamic loader can find it regardless of the RUNPATH baked in at build
# time (which points at the absolute build dir on the machine that compiled it).
locate_lib() {
    for d in "$(dirname "$SPNPEER")" "$repo/build" "$repo"; do
        if [ -f "$d/libskypeernettcp.so" ]; then echo "$d"; return; fi
    done
    return 1
}
LIBDIR="$(locate_lib || true)"
if [ -z "$LIBDIR" ]; then
    echo "ERROR: libskypeernettcp.so not found near $SPNPEER." >&2
    echo "Build it first:  (cd '$repo' && mkdir -p build && cd build && cmake .. && make)" >&2
    exit 1
fi
export LD_LIBRARY_PATH="$LIBDIR:${LD_LIBRARY_PATH:-}"

mkcfg() { # $1=nodedir  $2=readpipe $3=writepipe [$4=tracker_port_override]
    local dir="$1" rp="$2" wp="$3" tport="${4:-$PORT}"
    mkdir -p "$dir/config" "$dir/finalized"
    sed -e "s|/tmp/skypeernet_read|$rp|" \
        -e "s|/tmp/skypeernet_write|$wp|" \
        -e "s|\"tracker_port\"[^,]*|\"tracker_port\"             : $tport|" \
        "$repo/config/settings.cfg" > "$dir/config/settings.cfg"
}

genkeys() { # $1=nodedir  -> prints pubkeyhash on stdout, diagnostics on stderr
    local dir="$1"
    if [ -f "$dir/.pub.key" ]; then           # already generated on a prior run
        sha256sum "$dir/.pub.key" | cut -c1-64
        return
    fi
    # The peer writes .priv.key/.pub.key during startup; it does not need the
    # tracker for that. Run it in the background and wait for the key to appear
    # (RSA keygen can block on entropy on headless servers, so be patient).
    ( cd "$dir" && exec "$SPNPEER" </dev/null >"$dir/keygen.log" 2>&1 ) &
    local pid=$!
    local i
    for i in $(seq 1 120); do                  # up to ~60s
        [ -f "$dir/.pub.key" ] && break
        kill -0 "$pid" 2>/dev/null || break    # peer exited early
        sleep 0.5
    done
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    if [ ! -f "$dir/.pub.key" ]; then
        echo "ERROR: $(basename "$dir") did not create keys (.pub.key)." >&2
        echo "Peer output ($dir/keygen.log):" >&2
        sed 's/^/    /' "$dir/keygen.log" >&2 2>/dev/null || true
        echo "Hints: is spnpeer built and runnable? On a low-entropy host RSA" >&2
        echo "keygen can stall — install haveged/rng-tools or retry." >&2
        exit 1
    fi
    sha256sum "$dir/.pub.key" | cut -c1-64
}

cmd_up() {
    local count="${1:-2}"
    mkdir -p "$BASE"
    echo "base: $BASE   tracker port: $PORT   peers: $count"

    # tracker
    mkcfg "$BASE/tracker" "/tmp/spn_ln_tracker_read" "/tmp/spn_ln_tracker_write"

    # peers: generate keys first so we can build the whitelist
    local names=() hashes=()
    for i in $(seq 1 "$count"); do
        # 65='A' -> peerA, peerB, ... (octal escape converts the code point)
        local name; name="peer$(printf "\\$(printf '%03o' $((64 + i)))")"
        names+=("$name")
        mkcfg "$BASE/$name" "/tmp/spn_ln_${name}_read" "/tmp/spn_ln_${name}_write"
        local h; h="$(genkeys "$BASE/$name")"
        hashes+=("$h")
        echo "  $name  $h"
    done

    # tracker whitelist (JSON array of all peer hashes)
    { printf '{"keys":['
      for j in "${!hashes[@]}"; do
          [ "$j" -gt 0 ] && printf ','
          printf '"%s"' "${hashes[$j]}"
      done
      printf ']}'; } > "$BASE/tracker/.whitelist"

    # clean stale pipes, then launch
    rm -f /tmp/spn_ln_*_read /tmp/spn_ln_*_write
    ( cd "$BASE/tracker" && setsid "$SPNTRACKER" </dev/null >"$BASE/tracker/log" 2>&1 & echo $! >"$BASE/tracker/pid" )
    sleep 1
    for name in "${names[@]}"; do
        ( cd "$BASE/$name" && setsid "$SPNPEER" </dev/null >"$BASE/$name/log" 2>&1 & echo $! >"$BASE/$name/pid" )
    done
    sleep 3

    echo
    echo "up. launch a GUI for each peer (separate terminals):"
    for name in "${names[@]}"; do
        echo "  python3 $repo/gui/app.py --peer-dir $BASE/$name"
    done
    echo
    echo "stop everything with:  $0 down"
}

cmd_down() {
    for f in "$BASE"/*/pid; do
        [ -f "$f" ] || continue
        kill "$(cat "$f")" 2>/dev/null || true
        rm -f "$f"
    done
    # belt and suspenders
    pkill -f "$SPNPEER" 2>/dev/null || true
    pkill -f "$SPNTRACKER" 2>/dev/null || true
    echo "stopped."
}

cmd_gui() {
    local name="${1:?usage: $0 gui <peerA|peerB|...>}"
    exec python3 "$repo/gui/app.py" --peer-dir "$BASE/$name"
}

case "${1:-}" in
    up)   shift; cmd_up "$@";;
    down) cmd_down;;
    gui)  shift; cmd_gui "$@";;
    *)    echo "usage: $0 {up [count] | down | gui <name>}" >&2; exit 1;;
esac
