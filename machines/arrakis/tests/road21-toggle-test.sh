#!/usr/bin/env bash
# Fake bodies are single-quoted on purpose: their $VARS expand when the
# fake runs, not when it is written.
# shellcheck disable=SC2016
# Desk test for arrakis's road21 dispatcher script, run against the built
# script with a fake nmcli:
#   bash machines/arrakis/tests/road21-toggle-test.sh "$(nix build --no-link --print-out-paths \
#     .#nixosConfigurations.arrakis.config.system.build.toplevel)/etc/NetworkManager/dispatcher.d/03userscript0001"
set -u

built=$(readlink -f "$1")
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0

# Fake nmcli. ACTIVE = newline-separated terse lines NAME:UUID:TYPE:DEVICE
mkdir "$tmp/bin"
# Absolute paths: the script runs with PATH=/var/empty, like the dispatcher
printf '#!%s\n%s\n' "$(command -v bash)" '
echo "nmcli $*" >> "$LOG"
case "$*" in
  "-t -f NAME connection show --active") printf "%s\n" "$ACTIVE" | CUT -d: -f1 ;;
  *"connection show --active"*) printf "%s\n" "$ACTIVE" ;;
  "networking connectivity") echo "${CONN:-full}" ;;
esac' | sed "s#CUT#$(command -v cut)#" > "$tmp/bin/nmcli"
chmod +x "$tmp/bin/nmcli"

# The built script pins PATH; put the fake first
sed "s#^export PATH=\"#export PATH=\"$tmp/bin:#" "$built" > "$tmp/toggle.sh"

lo='lo:u0:loopback:lo'
home='Anor Londovercourt:u1:802-11-wireless:wlp1s0'
cafe='Cafe Wifi:u2:802-11-wireless:wlp1s0'
tun='road21:u3:wireguard:road21'

run() { # label expected iface action active-lines...
  local label=$1 want=$2 iface=$3 action=$4
  shift 4
  : > "$tmp/log"
  ACTIVE=$(printf '%s\n' "$@") LOG=$tmp/log PATH=/var/empty \
    "$(command -v bash)" "$tmp/toggle.sh" "$iface" "$action" 2> "$tmp/err"
  local rc=$? got
  got=$(grep -E 'connection (up|down) road21' "$tmp/log" | head -1)
  got=${got:-none}
  if [ "$rc" = 0 ] && [ "$got" = "$want" ]; then
    echo "PASS $label"
  else
    echo "FAIL $label: exit=$rc got='$got' want='$want' $(head -1 "$tmp/err")"
    fails=$((fails + 1))
  fi
}

# Changes must not wait for NetworkManager (--wait 0): NM holds
# activations until the dispatcher answers, which is busy running us.
up='nmcli --wait 0 connection up road21'
down='nmcli --wait 0 connection down road21'

run "home, up, tunnel down" none wlp1s0 up "$home" "$lo"
run "arrive home, tunnel up" "$down" wlp1s0 up "$home" "$tun" "$lo"
run "cafe, up" "$up" wlp1s0 up "$cafe" "$lo"
run "cafe, tunnel already up" none wlp1s0 connectivity-change "$cafe" "$tun" "$lo"
run "Wi-Fi off" none wlp1s0 down "$lo"
run "Wi-Fi off, tunnel lingering" none wlp1s0 down "$tun" "$lo"
run "road21 own event" none road21 up "$cafe" "$lo"
run "irrelevant action" none wlp1s0 dhcp4-change "$cafe" "$lo"

[ "$fails" = 0 ]
