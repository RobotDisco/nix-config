#!/bin/sh

numlockx on
display -window root -backdrop ~/.xmonad/bg
xscreensaver -no-splash &
pulseaudio -D &
parcellite -d &
exec xmonad
