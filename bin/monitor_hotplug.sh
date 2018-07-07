#!/bin/bash

# Bail if error
set -o errexit
set -o pipefail
set -o nounset
#set -o xtrace

# My external monitors bind to /sys/class/drm/card0-DP-3 and card0-DP-4
# Of course these have no relation to XRandR devices :-)
# What I'll do is detect whether they exist, and if so, do xrandr command set A
# If they don't, assume I'm back to my single monitor screen.
# Lord help me if I ever want to play with other external monitors >.>

# Does it look like my external monitors are connected?
if [[ $(xrandr | grep "^DP-1-2" | grep -c disconnected) -eq 0 ]]; then
    /usr/bin/xrandr --output DP-1-1 --auto
    /usr/bin/xrandr --output DP-1-2 --auto --right-of DP-1-1 --rotate left
    /usr/bin/xrandr --output eDP-1 --off
    source /home/gaelan/.fehbg
    /usr/bin/notify-send --urgency low -t 5000 "Graphics Update" \
			 "External Monitors connected"
else
    # We must have unplugged my external monitors, make sure
    # embedded display is enabled
    /usr/bin/xrandr --output DP-1-1 --off
    /usr/bin/xrandr --output DP-1-2 --off
    /usr/bin/xrandr --output eDP-1 --auto
    source /home/gaelan/.fehbg
    /usr/bin/notify-send --urgency low -t 5000 "Graphics Update" \
			 "External Monitors disconnected"
fi
