#!/usr/bin/env bash
# Each case is a condition whose status check() records via $?
# shellcheck disable=SC2319
# Desk test for road21-hosts.sh. Runs anywhere with bash and awk:
#   nix shell nixpkgs#gawk -c bash machines/fountain-of-ahmed-iii/tests/road21-hosts-test.sh
set -u

script="$(dirname "$0")/../road21-hosts.sh"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0

# Run the script the way writeShellApplication does
hosts() { bash -euo pipefail "$script" "$@"; }

check() { # name, condition result
  if [ "$2" = 0 ]; then echo "PASS $1"; else echo "FAIL $1"; fails=$((fails + 1)); fi
}

base=$'127.0.0.1 localhost\n10.1.2.3 corp.example\n'
want_block=$'# BEGIN road21 (nix-darwin)\n192.168.50.99 vaultwarden.robot-disco.net\n192.168.50.99 fallcube.robot-disco.net\n# END road21 (nix-darwin)\n'
names=(192.168.50.99 vaultwarden.robot-disco.net fallcube.robot-disco.net)

f=$tmp/adds
printf '%s' "$base" > "$f"
hosts "$f" "${names[@]}"
[ "$(cat "$f"; echo x)" = "$base${want_block}x" ]
check "adds block" $?

f=$tmp/idem
printf '%s' "$base" > "$f"
hosts "$f" "${names[@]}"
cp "$f" "$tmp/idem.once"
hosts "$f" "${names[@]}"
cmp -s "$f" "$tmp/idem.once" && [ "$(grep -c '^# BEGIN road21' "$f")" = 1 ]
check "idempotent" $?

f=$tmp/repl
printf '%s' "$base" > "$f"
hosts "$f" "${names[@]}"
hosts "$f" 192.168.50.99 vaultwarden.robot-disco.net
[ "$(grep -c '^192.168.50.99 ' "$f")" = 1 ] && ! grep -q fallcube "$f"
check "replaces" $?

f=$tmp/rm
printf '%s' "$base" > "$f"
cp "$f" "$tmp/rm.orig"
hosts "$f" "${names[@]}"
grep -q '^# BEGIN road21' "$f" && hosts "$f" && cmp -s "$f" "$tmp/rm.orig"
check "removes" $?

f=$tmp/nonl
printf '127.0.0.1 localhost' > "$f"
hosts "$f" "${names[@]}"
[ "$(head -2 "$f")" = $'127.0.0.1 localhost\n# BEGIN road21 (nix-darwin)' ]
check "no trailing newline" $?

f=$tmp/mode
printf '%s' "$base" > "$f"
chmod 640 "$f"
hosts "$f" "${names[@]}"
grep -q '^# BEGIN road21' "$f" &&
  [ "$(stat -c %a "$f" 2>/dev/null || stat -f %Lp "$f")" = 640 ]
check "keeps mode" $?

[ "$fails" = 0 ]
