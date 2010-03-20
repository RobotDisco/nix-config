#!/bin/zsh

usage () {
  echo "Usage: <new_suspend_dir> <new_coroner_file>"
}

if [[ -z $1 || -z $2 || ! -a $2 ]]; then
  usage
  exit 1
fi

cp -av ~/sr/coroner/suspend_files ./$1
coroner --directory=$1 $2
tar cvf $1.tar $1/*

