# NetworkManager dispatcher hook, called as `road21-toggle <interface>
# <action>`. At home (the "Anor Londovercourt" connection) road21 goes
# down; on any other fully activated network it comes up.

iface=${1-}
action=${2-}

# road21's own up/down events must not re-toggle it
[ "$iface" = road21 ] && exit 0
case $action in
  up | down | connectivity-change) ;;
  *) exit 0 ;;
esac

# Prints "home", "away" or "offline" (offline = do nothing).
# TODO: captive portals count as "away", so the tunnel comes up
# before the login page; `nmcli connection down road21` to log
# in. Fixing it needs NM's connectivity check, which is off
# (no uri), plus an `elif` on `nmcli networking connectivity`.
location() {
  local nmoutput;

  nmoutput="$(nmcli -f name,type,state --terse connection show --active)" || { echo offline; return; }

  if ! echo "$nmoutput" | grep -v "loopback" | grep -e ":activated$" >/dev/null; then
    echo offline
  elif grep -q "Anor Londovercourt" <<<"$nmoutput"; then
    echo home
  else
    echo away
  fi
}

# Not `nmcli | grep -q`: under pipefail, grep -q exiting early
# makes nmcli die of SIGPIPE and the pipeline report failure.
active() {
  grep -qx road21 <<<"$(nmcli -t -f NAME connection show --active)"
}

# Only act on a change, so re-runs never bounce the tunnel. --wait 0:
# never wait on NetworkManager, which holds activations
# until the dispatcher (busy running us) answers.
case $(location) in
  home) if active; then nmcli --wait 0 connection down road21; fi ;;
  away) if ! active; then nmcli --wait 0 connection up road21; fi ;;
esac
