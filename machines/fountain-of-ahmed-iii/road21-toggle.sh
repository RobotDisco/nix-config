# Run by launchd every 30 s. At home (default gateway is stargate) every
# road21 profile goes down; away, road21 comes up unless road21-full is
# up. Whichever is up gets its endpoint re-resolved when the handshake
# goes stale, since stargate's IP changes (DDNS) and wg-quick only
# resolves it once.

HOME_GW_MAC=64:62:66:2f:7a:71
ENDPOINT_HOST=robot-disco.net
ENDPOINT_PORT=51820
# WireGuard re-handshakes every 2 minutes; older than this means stale
STALE_AFTER=135
WG_RUN_DIR=${WG_RUN_DIR:-/var/run/wireguard}

log() { echo "$(date +%s) $*"; }

# wg-quick writes <name>.name (holding the utun) while a profile is up
is_up() { [ -e "$WG_RUN_DIR/$1.name" ]; }

# Default route's gateway and interface. Jamf's default is
# interface-scoped and road21-full uses 0/1 + 128/1, so neither shows up
# here.
route_info=$(route -n get default 2>/dev/null || true)
gw=$(awk '/gateway:/ { print $2 }' <<<"$route_info")
ifc=$(awk '/interface:/ { print $2 }' <<<"$route_info")

# Prints "home", "away" or "unknown" (unknown = do nothing). The SSID is
# redacted on macOS 27, so home is recognised by stargate's MAC.
home_state() {
  if [ -z "$gw" ]; then
    echo unknown
    return
  fi
  local mac
  mac=$(arp -n "$gw" 2>/dev/null | awk '{ print $4 }' || true)
  # Anything but a MAC ("(incomplete)", or "no" from "-- no entry" once
  # the ARP entry expires) says nothing about where we are
  if [ "$mac" = "$HOME_GW_MAC" ]; then
    echo home
  elif [[ $mac =~ ^[0-9a-f]{1,2}(:[0-9a-f]{1,2}){5}$ ]]; then
    echo away
  else
    echo unknown
  fi
}

# A profile whose wireguard-go has died leaves its .name file behind but
# its utun is gone (the utun lives only as long as wireguard-go). Clear it
# so it reads as down and gets brought up again.
for profile in road21 road21-full; do
  name_file=$WG_RUN_DIR/$profile.name
  if [ -e "$name_file" ] && ! ifconfig "$(cat "$name_file")" >/dev/null 2>&1
  then
    log "$profile: interface is gone, clearing"
    wg-quick down "$profile" || true
    rm -f "$name_file"
  fi
done

# Re-point a profile's peer at stargate's current address if the
# handshake is stale
reresolve() {
  local profile=$1 iface key handshake dns ip
  iface=$(cat "$WG_RUN_DIR/$profile.name")
  key=$(wg show "$iface" peers)
  handshake=$(wg show "$iface" latest-handshakes | awk '{ print $2 }')
  if [ $(($(date +%s) - handshake)) -le "$STALE_AFTER" ]; then
    return
  fi

  # Ask the local network's DNS server: under road21-full all DNS goes to
  # stargate through the tunnel, which is what's broken
  dns=$(ipconfig getoption "$ifc" domain_name_server 2>/dev/null || true)
  if [ -z "$dns" ]; then
    log "$profile: stale handshake, no DHCP DNS server to ask"
    return
  fi
  # An off-link DNS server (8.8.8.8, an ISP resolver) would be reached
  # through road21-full's 0/1 + 128/1, i.e. the dead tunnel: route it via
  # the real gateway for the lookup only
  local via_gw=""
  if [ "$(route -n get "$dns" 2>/dev/null | awk '/interface:/ { print $2 }')" = "$iface" ]; then
    route -n add -host "$dns" "$gw" >/dev/null 2>&1 && via_gw=1
  fi
  ip=$(dig +short @"$dns" "$ENDPOINT_HOST" A | grep -E '^[0-9.]+$' | tail -1 || true)
  if [ -n "$via_gw" ]; then
    route -n delete -host "$dns" >/dev/null 2>&1 || true
  fi
  if [ -z "$ip" ]; then
    log "$profile: stale handshake, $ENDPOINT_HOST did not resolve"
    return
  fi

  log "$profile: stale handshake, endpoint now $ip"
  wg set "$iface" peer "$key" endpoint "$ip:$ENDPOINT_PORT"
  if [ "$profile" = road21-full ]; then
    # Without a host route via the real gateway, packets to the new
    # endpoint would follow 0/1 into the tunnel itself
    route -n add -host "$ip" "$gw" 2>/dev/null || true
  fi
}

case $(home_state) in
  home)
    for profile in road21 road21-full; do
      if is_up "$profile"; then
        log "home: bringing $profile down"
        wg-quick down "$profile"
      fi
    done
    ;;
  away)
    if is_up road21-full; then
      if is_up road21; then
        log "away: road21-full is up, bringing road21 down"
        wg-quick down road21
      fi
      reresolve road21-full
    elif ! is_up road21; then
      # No handshake yet; the next run checks it
      log "away: bringing road21 up"
      wg-quick up road21
    else
      reresolve road21
    fi
    ;;
esac
