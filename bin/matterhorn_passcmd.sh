#! /usr/bin/env bash

#set -o xtrace
set -o errexit
set -o pipefail
set -o nounset

gpg -d < $HOME/.passwd/tulip-passcore.gpg
