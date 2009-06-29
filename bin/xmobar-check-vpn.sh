#!/bin/sh

# xmobar-check-vpn.sh
# xmobar helper, prints output if vpnc is active
# Gaelan D'costa, 2009

VPN_ACTIVE=$(pgrep 'vpnc')

if [ -z "$VPN_ACTIVE" ]; then
	echo ""
else
	echo "(VPN)"
fi
