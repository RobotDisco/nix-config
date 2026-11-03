# Run by a systemd timer every 30 s. A full tunnel that can't handshake
# takes all internet with it, so: reconnect road21 once (NetworkManager
# re-resolves the endpoint, which follows DDNS), and if that doesn't help
# either, take it down. The dispatcher tries again on the next network
# change. --wait 0 so nothing here waits on NetworkManager.

# Allow this long after road21 comes up for the first handshake
START_GRACE=60
# WireGuard re-handshakes every 2 minutes; older than this means stale
STALE_AFTER=180
STATE_DIR=${STATE_DIR:-/run/road21-watchdog}

log() { echo "road21-watchdog: $*"; }

mkdir -p "$STATE_DIR"
up_since=$STATE_DIR/up-since
retried=$STATE_DIR/retried

if ! grep -qx road21 <<<"$(nmcli -t -f NAME connection show --active)"; then
  rm -f "$up_since" "$retried"
  exit 0
fi

# Unreadable values mean "skip this run", never "act"
is_number() { [[ $1 =~ ^[0-9]+$ ]]; }

now=$(date +%s)
since=$(cat "$up_since" 2>/dev/null || true)
if ! is_number "$since"; then
  # First sight, or a garbled file: start counting from now
  echo "$now" > "$up_since"
  exit 0
fi

if ! handshakes=$(wg show road21 latest-handshakes 2>/dev/null); then
  log "can't read road21's handshake, skipping this run"
  exit 0
fi
# No peer at all prints nothing: that's "never", which the checks below
# treat like any other missing handshake
handshake=$(awk '{ print $2 }' <<<"$handshakes")
handshake=${handshake:-0}
if ! is_number "$handshake"; then
  log "unexpected handshake value, skipping this run"
  exit 0
fi

if [ "$handshake" -gt 0 ] && [ $((now - handshake)) -le "$STALE_AFTER" ]; then
  rm -f "$retried"
  exit 0
fi

if [ "$handshake" -eq 0 ] && [ $((now - since)) -le "$START_GRACE" ]; then
  exit 0
fi

if [ ! -e "$retried" ]; then
  log "no handshake from stargate, reconnecting once"
  touch "$retried"
  echo "$now" > "$up_since"
  nmcli --wait 0 connection up road21
else
  log "still no handshake after reconnecting, bringing road21 down"
  rm -f "$up_since" "$retried"
  nmcli --wait 0 connection down road21
fi
