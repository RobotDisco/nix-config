#!/bin/zsh

usage () {
  echo "Usage: $0 <path-to-suspend-files>"
}

if [[ -z $1 || ! -r $1  ]]; then
  usage
  exit 1
fi

abspath=$(readlink -f $1)
pushd ~/sr/coroner
./bochs -r $abspath &
popd
