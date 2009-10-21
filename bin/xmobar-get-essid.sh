#!/bin/sh

# xmobar-get-essid.sh
# xmobar helper, prints wireless ssid
# Gaelan D'costa, 2009

ESSID=$(iwgetid --raw)

if [ -z "$ESSID" ]; then
  echo "-"
else
  echo "$ESSID"
fi
