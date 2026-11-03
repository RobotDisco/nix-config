#!/usr/bin/env bash
# Fake bodies are single-quoted on purpose: their $VARS expand when the
# fake runs, not when it is written.
# shellcheck disable=SC2016
# Desk test for arrakis's road21 dispatcher script with a fake nmcli:
#   bash machines/arrakis/tests/road21-toggle-test.sh
# or against the built script (pinned PATH and all):
#   bash machines/arrakis/tests/road21-toggle-test.sh "$(nix build --no-link --print-out-paths \
#     .#nixosConfigurations.arrakis.config.system.build.toplevel)/etc/NetworkManager/dispatcher.d/03userscript0001"
set -u

built=${1:+$(readlink -f "$1")}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0

mkdir "$tmp/bin"
# Fake nmcli. ACTIVE = newline-separated lines NAME:UUID:TYPE:DEVICE:STATE.
# `connection show --active` prints the fields asked for with -f/--fields
# (default NAME,UUID,TYPE,DEVICE, as the real nmcli does).
# Absolute paths: the script runs with PATH=/var/empty, like the dispatcher
printf '#!%s\n%s\n' "$(command -v bash)" '
echo "nmcli $*" >> "$LOG"
args=("$@")
# Reject options the real nmcli does not know (it exits 2)
for a in "$@"; do
  case $a in
    -t | --terse | -f | --fields | -g | --get-values | --wait | --active) ;;
    -*) echo "Error: Option '"'"'$a'"'"' is unknown, try '"'"'nmcli -help'"'"'." >&2; exit 2 ;;
  esac
done
case "$*" in
  *"connection show --active"*)
    fields=NAME,UUID,TYPE,DEVICE
    for i in "${!args[@]}"; do
      case ${args[i]} in -f | --fields) fields=${args[i + 1]} ;; esac
    done
    printf "%s\n" "$ACTIVE" | AWK -F: -v want="$fields" '"'"'
      BEGIN { n = split("NAME UUID TYPE DEVICE STATE", col, " ")
              for (i = 1; i <= n; i++) idx[col[i]] = i
              m = split(toupper(want), w, ",") }
      { line = ""
        for (i = 1; i <= m; i++) line = line (i > 1 ? ":" : "") $idx[w[i]]
        print line }'"'"' ;;
  "networking connectivity") echo "${CONN:-full}" ;;
esac' | sed "s#AWK#$(command -v awk)#" > "$tmp/bin/nmcli"
chmod +x "$tmp/bin/nmcli"

if [ -n "$built" ]; then
  # The built script pins PATH and sets the shell options; put the fake first
  sed "s#^export PATH=\"#export PATH=\"$tmp/bin:#" "$built" > "$tmp/toggle.sh"
else
  # The source file, with what writeShellApplication adds: shell options
  # and a PATH of the fake plus grep (runtimeInputs)
  {
    echo 'set -o errexit -o nounset -o pipefail'
    echo "export PATH=\"$tmp/bin:$(dirname "$(command -v grep)")\""
    cat "$(dirname "$0")/../road21-toggle.sh"
  } > "$tmp/toggle.sh"
fi

lo='lo:u0:loopback:lo:activated'
home='Anor Londovercourt:u1:802-11-wireless:wlp1s0:activated'
cafe='Cafe Wifi:u2:802-11-wireless:wlp1s0:activated'
# Mid-switch: the new Wi-Fi is listed as active but has no DNS yet
cafe_activating='Cafe Wifi:u2:802-11-wireless:wlp1s0:activating'
tun='road21:u3:wireguard:road21:activated'

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
# Bringing road21 up now would resolve its endpoint without DNS and leave
# it dead; wait for the up event once the new Wi-Fi is activated
run "mid-switch, new Wi-Fi activating" none wlp1s0 down "$cafe_activating" "$lo"

[ "$fails" = 0 ]
