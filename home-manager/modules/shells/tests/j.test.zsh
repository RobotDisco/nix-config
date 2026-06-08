#!/usr/bin/env zsh
# Tests for the j() autoloaded function.
#
# Run directly:  zsh tests/j.test.zsh
# Via just:      just test-zsh
#
# Strategy: build a tmp fixture tree, set $cdpath to point at it, autoload
# j from the in-repo files/functions/, then call j and observe $PWD.

emulate -L zsh
# Deliberately NOT setting err_return / err_exit: a test runner must
# survive individual failures so it can report all of them. Note also
# that ((expr)) returns failure when expr evaluates to zero — so e.g.
# `(( counter++ ))` "fails" on first call (returns old value 0). That's
# fine here because we don't trip on it.
setopt extended_glob null_glob

# ----- resolve paths ------------------------------------------------------
# ${0:A:h} -- absolute path, then directory of this script.
local script_dir=${0:A:h}
local module_root=${script_dir:h}
local functions_dir=${module_root}/files/functions


# ----- harness ------------------------------------------------------------
local -i tests_run=0 tests_passed=0 tests_failed=0
local current_test=""

it() {
  current_test=$1
  (( tests_run++ ))
}

ok() {
  print "  ✓ $current_test"
  (( tests_passed++ ))
}

fail() {
  print "  ✗ $current_test"
  local line
  for line in "$@"; do
    print "      $line"
  done
  (( tests_failed++ ))
}

# Assert that PWD matches the expected path (resolved, in case of symlinks).
assert_pwd() {
  local expected=${1:A}
  local actual=${PWD:A}
  if [[ $actual == $expected ]]; then
    ok
  else
    fail "expected PWD: $expected" "actual PWD:   $actual"
  fi
}

# Assert that running the given command fails (non-zero exit).
# Suppress its stderr so test output stays clean.
assert_fails() {
  if "$@" 2>/dev/null; then
    fail "expected '$*' to fail, but it succeeded"
  else
    ok
  fi
}


# ----- fixture ------------------------------------------------------------
local fixture
fixture=$(mktemp -d)
# Clean up the fixture no matter how the script exits.
trap "rm -rf '$fixture'" EXIT INT TERM

mkdir -p $fixture/alpha
mkdir -p $fixture/beta
mkdir -p $fixture/nested/charlie
mkdir -p $fixture/nested/charlie-old
mkdir -p $fixture/other/charlie-clone

# Point j() at the fixture via the J_ROOTS environment override.
J_ROOTS=( $fixture )

# Load j from the in-repo source (not the home-manager-installed copy).
fpath=( $functions_dir $fpath )
autoload -Uz j


# ----- cases --------------------------------------------------------------
print "j() tests:"

it "errors with usage when called without an argument"
assert_fails j

it "errors when no directory matches the query"
cd $fixture
assert_fails j definitely-not-a-dir-name

it "cds to a unique top-level match"
cd $fixture
j alpha
assert_pwd $fixture/alpha

it "matches a substring in a nested directory"
cd $fixture
j old                                  # only charlie-old contains "old"
assert_pwd $fixture/nested/charlie-old

it "matches case-insensitively"
cd $fixture
j ALPHA
assert_pwd $fixture/alpha


# --- future evolution: uncomment when you add recency-weighted ranking ---
#
# it "prefers more-recently-visited directories on ambiguous matches"
# cd $fixture/other/charlie-clone      # least recent
# cd $fixture/nested/charlie-old
# cd $fixture/nested/charlie           # newest
# cd $fixture
# j charlie                            # ambiguous: 3 candidates
# assert_pwd $fixture/nested/charlie   # the most recent wins


# ----- summary ------------------------------------------------------------
print ""
if (( tests_failed > 0 )); then
  print "  $tests_run tests, $tests_passed passed, $tests_failed failed"
  exit 1
else
  print "  $tests_run tests, all passed"
fi
