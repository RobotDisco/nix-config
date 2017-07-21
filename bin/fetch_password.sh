#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# if no arguments given, fail

# If deployer doesn't exist, complain

scp deploy.tulip.prod:workspace/deployer/config/stages/$1 ~/workspace/deployer/config/stages/$1
