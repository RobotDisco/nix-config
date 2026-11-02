#!/usr/bin/env bash
# Fake bodies are single-quoted on purpose: their $VARS expand when the
# fake runs, not when it is written.
# shellcheck disable=SC2016
# Desk test for road21-toggle.sh with fake macOS and WireGuard commands.
# Runs anywhere with bash and awk:
#   nix shell nixpkgs#gawk -c bash machines/fountain-of-ahmed-iii/tests/road21-toggle-test.sh
set -u

script="$(dirname "$0")/../road21-toggle.sh"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0
home=64:62:66:2f:7a:71

# Fakes print what macOS prints, driven by env vars, and log actions
mkdir "$tmp/bin"
fake() { printf '#!/usr/bin/env bash\n%s\n' "$2" > "$tmp/bin/$1"; chmod +x "$tmp/bin/$1"; }
fake route '
if [ "$1 $2" = "-n get" ]; then
  [ -n "$GW" ] || { echo "route: writing to routing socket: not in table" >&2; exit 1; }
  printf "   route to: default\ndestination: default\n    gateway: %s\n  interface: %s\n" "$GW" "$IFC"
else echo "route $*" >> "$LOG"; fi'
fake arp '
[ -n "$MAC" ] || { echo "$2 ($2) -- no entry"; exit 1; }
echo "? ($2) at $MAC on $IFC ifscope [ethernet]"'
fake wg '
case "$1 $3" in
  "show peers") echo KEY ;;
  "show latest-handshakes") printf "KEY\t%s\n" "$HANDSHAKE" ;;
  *) echo "wg $*" >> "$LOG" ;;
esac'
fake wg-quick 'echo "wg-quick $*" >> "$LOG"'
# Interfaces in LIVE exist; any other is gone, as when wireguard-go dies
fake ifconfig '
case " $LIVE " in
  *" $1 "*) echo "$1: flags=8051<UP,POINTOPOINT,RUNNING,MULTICAST> mtu 1412" ;;
  *) echo "ifconfig: interface $1 does not exist" >&2; exit 1 ;;
esac'
fake ipconfig '[ -n "$DNS" ] && echo "$DNS" || exit 1'
fake dig '[ -n "$DIG_IP" ] && printf "stargate.example.\n%s\n" "$DIG_IP"; exit 0'
fake date 'echo "$NOW"'

run() { # name expected [VAR=value...]
  local name=$1 want=$2
  shift 2
  rm -rf "$tmp/run" && mkdir "$tmp/run"
  : > "$tmp/log"
  local -A env=([GW]=192.168.1.1 [IFC]=en0 [MAC]=aa:bb:cc:dd:ee:ff [UP]=""
    [HANDSHAKE]=1000 [NOW]=1010 [DNS]=192.168.1.1 [DIG_IP]=203.0.113.7
    [LIVE]=utun9)
  local kv
  for kv in "$@"; do env[${kv%%=*}]=${kv#*=}; done
  local p
  for p in ${env[UP]}; do echo utun9 > "$tmp/run/$p.name"; done
  local rc=0
  env GW="${env[GW]}" IFC="${env[IFC]}" MAC="${env[MAC]}" \
    HANDSHAKE="${env[HANDSHAKE]}" NOW="${env[NOW]}" DNS="${env[DNS]}" \
    DIG_IP="${env[DIG_IP]}" LIVE="${env[LIVE]}" \
    LOG="$tmp/log" WG_RUN_DIR="$tmp/run" \
    PATH="$tmp/bin:$PATH" bash -euo pipefail "$script" > /dev/null 2>&1 || rc=$?
  local got
  got=$(paste -sd';' "$tmp/log")
  if [ "$rc" = 0 ] && [ "$got" = "$want" ]; then
    echo "PASS $name"
  else
    echo "FAIL $name: exit=$rc got='$got' want='$want'"
    fails=$((fails + 1))
  fi
}

run "home, road21 up" "wg-quick down road21" MAC=$home UP=road21
run "home, full up" "wg-quick down road21-full" MAC=$home UP=road21-full
run "home, none up" "" MAC=$home
run "away, none up" "wg-quick up road21"
run "away, road21 fresh" "" UP=road21
run "away, both up" "wg-quick down road21" UP="road21 road21-full"
run "road21 stale" "wg set utun9 peer KEY endpoint 203.0.113.7:51820" \
  UP=road21 NOW=1200
run "full stale" \
  "wg set utun9 peer KEY endpoint 203.0.113.7:51820;route -n add -host 203.0.113.7 10.0.0.1" \
  UP=road21-full NOW=1200 GW=10.0.0.1
run "no default route" "" GW=
run "arp incomplete" "" "MAC=(incomplete)"
run "arp no entry" "" MAC=
run "arp no entry, road21 up" "" MAC= UP=road21
run "home, road21 interface gone" "wg-quick down road21" \
  MAC=$home UP=road21 LIVE=
run "away, road21 interface gone" "wg-quick down road21;wg-quick up road21" \
  UP=road21 LIVE=
run "away, full interface gone" \
  "wg-quick down road21-full;wg-quick up road21" UP=road21-full LIVE=
run "no dhcp dns" "" UP=road21 NOW=1200 DNS=
run "dig empty" "" UP=road21 NOW=1200 DIG_IP=

[ "$fails" = 0 ]
