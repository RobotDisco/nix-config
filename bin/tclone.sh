#!/usr/bin/env sh

set -uex

mkdir -p $(dirname ~/workspace/$1)
git clone git@git.internal.tulip.io:$1 ~/workspace/$1
