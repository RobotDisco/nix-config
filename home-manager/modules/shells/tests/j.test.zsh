#!/usr/bin/env zsh
set -eu -o pipefail

# Gotta source your function to use it.
source "$(dirname $0)/../files/functions/j"

export XDG_DATA_HOME=$(mktemp -d)
data_dir="$XDG_DATA_HOME/j"
histfile="$XDG_DATA_HOME/j/history"

mkdir -p "$data_dir"

printf "0 1 /bob\n1 2 /sally\n2 3 /jill\n" > "$histfile"

actual=$(j)
expected=$'2 3 /jill\n1 2 /sally\n0 1 /bob'
if [[ "$actual" != "$expected" ]]; then
    print "FAIL: expected '$expected', got '$actual'"
    exit 1
fi


printf "2 3 /bob\n1 2 /sally\n0 1 /jill\n" > "$histfile"

actual=$(j)
expected=$'2 3 /bob\n1 2 /sally\n0 1 /jill'
if [[ "$actual" != "$expected" ]]; then
    print "FAIL: expected '$expected', got '$actual'"
    exit 1
fi

dir1="$XDG_DATA_HOME/fixtures/bob1"
mkdir -p "$dir1"

printf "0 1 %s\n" "$dir1" > "$histfile"

j bob
if [[ "$PWD" != "$dir1" ]]; then
    print "FAIL: expected PWD=$dir1, got $PWD"
    exit 1
fi

dir2="$XDG_DATA_HOME/fixtures/bob2"
mkdir -p "$dir2"

j "$dir2"
if ! grep -q "$dir2" "$histfile"; then
    print "FAIL: expected $dir2 in histfile"
    exit 1
fi
if [[ "$PWD" != "$dir2" ]]; then
    print "FAIL: expected PWD=$dir2, got $PWD"
    exit 1
fi
