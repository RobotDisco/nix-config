#!/bin/sh

numlockx on
display -window root -backdrop ~/.xmonad/bg
xscreensaver -no-splash &
parcellite -d &
exec xmonad
