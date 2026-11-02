#!/usr/bin/env bash
# Fake bodies are single-quoted on purpose: their $VARS expand when the
# fake runs, not when it is written.
# shellcheck disable=SC2016
# Desk test for road21-watchdog.sh with fake nmcli, wg and date:
#   nix shell nixpkgs#gawk -c bash machines/arrakis/tests/road21-watchdog-test.sh
set -u

script="$(dirname "$0")/../road21-watchdog.sh"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0

mkdir "$tmp/bin"
fake() { printf '#!%s\n%s\n' "$(command -v bash)" "$2" > "$tmp/bin/$1"; chmod +x "$tmp/bin/$1"; }
fake nmcli '
case "$*" in
  "-t -f NAME connection show --active") printf "lo\n"; [ "$ACTIVE" = yes ] && printf "road21\n"; exit 0 ;;
  *) echo "nmcli $*" >> "$LOG" ;;
esac'
fake wg 'printf "KEY\t%s\n" "$HANDSHAKE"'
fake date 'echo "$NOW"'

# run label expected-action expected-state [VAR=value...]
#   expected-state: "up-since retried" contents after the run, "-" = absent
run() {
  local label=$1 want=$2 want_state=$3
  shift 3
  local state=$tmp/state
  rm -rf "$state" && mkdir "$state"
  : > "$tmp/log"
  local -A v=([ACTIVE]=yes [HANDSHAKE]=0 [NOW]=1000 [UP_SINCE]=- [RETRIED]=-)
  local kv
  for kv in "$@"; do v[${kv%%=*}]=${kv#*=}; done
  [ "${v[UP_SINCE]}" = - ] || echo "${v[UP_SINCE]}" > "$state/up-since"
  [ "${v[RETRIED]}" = - ] || touch "$state/retried"
  local rc=0
  ACTIVE=${v[ACTIVE]} HANDSHAKE=${v[HANDSHAKE]} NOW=${v[NOW]} LOG=$tmp/log \
    STATE_DIR=$state PATH="$tmp/bin:$PATH" \
    bash -euo pipefail "$script" > /dev/null 2> "$tmp/err" || rc=$?
  local got got_state
  got=$(grep -E 'connection (up|down) road21' "$tmp/log" | head -1)
  got=${got:-none}
  got_state="$(cat "$state/up-since" 2>/dev/null || echo -) $([ -e "$state/retried" ] && echo yes || echo -)"
  if [ "$rc" = 0 ] && [ "$got" = "$want" ] && [ "$got_state" = "$want_state" ]; then
    echo "PASS $label"
  else
    echo "FAIL $label: exit=$rc got='$got' state='$got_state' want='$want' state='$want_state' $(head -1 "$tmp/err")"
    fails=$((fails + 1))
  fi
}

up='nmcli --wait 0 connection up road21'
down='nmcli --wait 0 connection down road21'

run "not active" none "- -" ACTIVE=no UP_SINCE=900 RETRIED=yes
run "first sight" none "1000 -"
run "starting" none "970 -" UP_SINCE=970
run "healthy" none "900 -" UP_SINCE=900 HANDSHAKE=990 RETRIED=yes
run "never handshook" "$up" "1000 yes" UP_SINCE=910
run "still nothing after retry" "$down" "- -" UP_SINCE=910 RETRIED=yes
run "went stale" "$up" "1000 yes" UP_SINCE=100 HANDSHAKE=700
run "stale after retry" "$down" "- -" UP_SINCE=100 HANDSHAKE=700 RETRIED=yes

[ "$fails" = 0 ]
