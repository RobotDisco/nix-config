# j TDD Rewrite Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task.
> Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rewrite `j` and `j.test.zsh` from blank slate using TDD, writing
every line yourself to learn zsh by doing.

**Architecture:** Test file first, raw `if/then` assertions, then extract
harness from repetition once you feel it. Implementation follows each test.
The test file autoloads `j` from the in-repo source, not the installed copy.

**Tech Stack:** zsh only — no external dependencies

## Global Constraints

- No code in either file that the user did not write themselves
- `j.test.zsh` must be directly runnable: `zsh .../j.test.zsh`
- `emulate -L zsh` at the top of every autoloaded function (isolates options)
- No copying from the deleted files

---

### Task 1: Blank slate

**Files:**
- Delete: `home-manager/modules/shells/files/functions/j`
- Delete: `home-manager/modules/shells/tests/j.test.zsh`
- Create: `home-manager/modules/shells/files/functions/j` (stub)
- Create: `home-manager/modules/shells/tests/j.test.zsh` (fixture only)

**Interfaces:**
- Produces: `j` exists and is autoloadable; `j.test.zsh` runs without errors

- [ ] **Step 1: Delete the existing files**

```bash
rm home-manager/modules/shells/files/functions/j
rm home-manager/modules/shells/tests/j.test.zsh
```

- [ ] **Step 2: Create an empty j stub**

Create `home-manager/modules/shells/files/functions/j` containing only:

```zsh
emulate -L zsh
```

`emulate -L zsh` resets all zsh options to their defaults for this function
only, then restores the caller's options on exit. The `-L` means "local" —
it is how you write isolated, portable zsh functions.

- [ ] **Step 3: Create j.test.zsh with fixture setup only**

This boilerplate wires up the fixture and loads `j` from the repo source.
Write it yourself — it teaches `${0:A:h}` path resolution, `mktemp`, `trap`,
and `fpath`/`autoload`:

```zsh
#!/usr/bin/env zsh
emulate -L zsh
setopt extended_glob null_glob

# ${0:A:h}: absolute path of this script (:A), then its directory (:h)
local script_dir=${0:A:h}
local module_root=${script_dir:h}
local functions_dir=${module_root}/files/functions

# Build fixture tree under a temp dir; clean up on any exit
local fixture
fixture=$(mktemp -d)
trap "rm -rf '$fixture'" EXIT INT TERM

mkdir -p $fixture/alpha
mkdir -p $fixture/beta
mkdir -p $fixture/nested/charlie

# Point j at the fixture instead of $HOME/workspace
J_ROOTS=( $fixture )

# Prepend the in-repo functions dir so autoload finds our source, not
# whatever home-manager installed
fpath=( $functions_dir $fpath )
autoload -Uz j

print "j() tests:"
```

- [ ] **Step 4: Verify the test file runs without errors**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected output: `j() tests:` then exits 0. No errors or warnings.

- [ ] **Step 5: Commit**

```bash
git add home-manager/modules/shells/files/functions/j \
        home-manager/modules/shells/tests/j.test.zsh
git commit -m "chore: blank slate for j TDD rewrite"
```

---

### Task 2: First test — single match (raw if/then)

**Files:**
- Modify: `home-manager/modules/shells/tests/j.test.zsh`

**Interfaces:**
- Consumes: `$fixture/alpha` from Task 1 fixture setup
- Produces: one failing test written in raw `if/then/print` — no helpers yet

- [ ] **Step 1: Write the first failing test**

Add this after the `print "j() tests:"` line. No helpers — write raw
`if/then`:

```zsh
cd $fixture
j alpha
if [[ ${PWD:A} == ${fixture:A}/alpha ]]; then
  print "  ✓ cds to a unique top-level match"
else
  print "  ✗ cds to a unique top-level match"
  print "    expected: ${fixture:A}/alpha"
  print "    got:      ${PWD:A}"
fi
```

Two things to notice:
- `${PWD:A}` — the `:A` modifier resolves symlinks to a canonical absolute
  path. Use it on both sides of directory comparisons; `/tmp` on macOS is
  a symlink and will cause false failures without it.
- `cd $fixture` before each test resets position so tests are independent.

- [ ] **Step 2: Run to confirm it fails**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `✗ cds to a unique top-level match` (j stub does nothing)

- [ ] **Step 3: Commit the failing test**

```bash
git add home-manager/modules/shells/tests/j.test.zsh
git commit -m "test: failing test for j single match"
```

---

### Task 3: Implement j — single match

**Files:**
- Modify: `home-manager/modules/shells/files/functions/j`

**Interfaces:**
- Consumes: `J_ROOTS` env var (array of root paths), `$1` as query string
- Produces: `cd` to the matching directory; single match case only

- [ ] **Step 1: Implement j to pass the first test**

Open `j` and write the minimum code to make the single-match test pass.
You need these primitives — wire them together yourself:

```
# Bind first arg to a local variable
local query=$1

# Declare a local array and populate from the env var
local -a roots
roots=( $J_ROOTS )

# Recursive glob with qualifiers — all on one line:
#   $^roots        rc-style expansion: distribute ** across each root
#   **             recursive descent into all subdirectories
#   *${~query}*    substring match; ${~query} forces glob interpretation
#                  of the variable contents (so foo* works as a glob)
#   (/N)           qualifier: (/) directories only, (N) null_glob
#                  (no error when the result is empty)
local -a matches
matches=( $^roots/**/*${~query}*(/N) )

# Array length and indexing (zsh arrays are 1-indexed):
$#matches          # number of elements
$matches[1]        # first element

# cd safely (-- stops option parsing so paths starting with - work)
cd -- $matches[1]
```

You also need `setopt local_options extended_glob` after `emulate -L zsh`
— `emulate` resets EXTENDED_GLOB to off, and `**` requires it.

- [ ] **Step 2: Run the test to confirm it passes**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `✓ cds to a unique top-level match`

- [ ] **Step 3: Commit**

```bash
git add home-manager/modules/shells/files/functions/j
git commit -m "feat: j single match"
```

---

### Task 4: Test + implement — missing argument error

**Files:**
- Modify: `home-manager/modules/shells/tests/j.test.zsh`
- Modify: `home-manager/modules/shells/files/functions/j`

**Interfaces:**
- Produces: `j` with no arguments exits non-zero and writes usage to stderr

- [ ] **Step 1: Write the failing test (raw if/then)**

Add after the first test block:

```zsh
cd $fixture
if j 2>/dev/null; then
  print "  ✗ errors with usage when called without an argument"
  print "    expected non-zero exit, got 0"
else
  print "  ✓ errors with usage when called without an argument"
fi
```

`2>/dev/null` silences stderr so error messages don't appear in test output.
The exit code is still captured by `if`.

- [ ] **Step 2: Run to confirm it fails**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `✗ errors with usage when called without an argument`

- [ ] **Step 3: Guard against empty query in j**

Add this near the top of `j`, after `emulate -L zsh` and `setopt`:

```zsh
local query=$1
if [[ -z $query ]]; then
  print -u2 'usage: j <query>'
  return 2
fi
```

`print -u2` writes to file descriptor 2 (stderr).
`return 2` is the convention for usage errors — distinct from `return 1`
(no match) so callers can tell the difference.

- [ ] **Step 4: Run to confirm both tests pass**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: both tests show `✓`

- [ ] **Step 5: Commit**

```bash
git add home-manager/modules/shells/tests/j.test.zsh \
        home-manager/modules/shells/files/functions/j
git commit -m "feat: j guards against missing argument"
```

---

### Task 5: Test + implement — no match error

**Files:**
- Modify: `home-manager/modules/shells/tests/j.test.zsh`
- Modify: `home-manager/modules/shells/files/functions/j`

**Interfaces:**
- Produces: `j definitely-not-a-dir` exits non-zero and writes error to stderr

- [ ] **Step 1: Write the failing test (raw if/then)**

```zsh
cd $fixture
if j definitely-not-a-dir-name 2>/dev/null; then
  print "  ✗ errors when no directory matches"
  print "    expected non-zero exit, got 0"
else
  print "  ✓ errors when no directory matches"
fi
```

By now you have three `if/then` blocks with near-identical structure.
Feel that repetition — it will matter in Task 6.

- [ ] **Step 2: Run to confirm it fails**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `✗ errors when no directory matches`

- [ ] **Step 3: Handle the no-match case in j**

After building `$matches`, add a `case` on its length. `case $#matches in`
switches on the array length — a zsh idiom for 0/1/many dispatch:

```
case $#matches in
  0) ... non-zero exit with message ...
  1) cd -- $matches[1] ;;
esac
```

`print -u2 "j: no match for '$query'"` is the message; `return 1` is the
exit code.

- [ ] **Step 4: Run to confirm all three tests pass**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: all three tests show `✓`

- [ ] **Step 5: Commit**

```bash
git add home-manager/modules/shells/tests/j.test.zsh \
        home-manager/modules/shells/files/functions/j
git commit -m "feat: j handles no-match case"
```

---

### Task 6: Extract the harness from repetition

**Files:**
- Modify: `home-manager/modules/shells/tests/j.test.zsh`

**Interfaces:**
- Produces: `it()`, `ok()`, `fail()`, `assert_pwd()` defined inline in
  `j.test.zsh`; all existing tests rewritten to use them; a summary at the end

- [ ] **Step 1: Look at what repeats**

You now have three raw `if/then` blocks. Each one:
- Has a description string duplicated in two `print` calls
- Uses the same pass/fail print format
- Two of them compare `${PWD:A}` to an expected path

That is the harness asking to be born. Write the helpers yourself.

- [ ] **Step 2: Add counter variables and helper functions**

Add these at the top of `j.test.zsh`, after the fixture setup block,
before the first test. Write them yourself using these primitives:

```
local -i tests_run=0 tests_passed=0 tests_failed=0
# local -i declares an integer; arithmetic on it works without $(()):
(( tests_run++ ))     # increment — returns 1 when value was 0, which is ok

local current_test=""

# it()   -- name the running test
# ok()   -- report pass, increment counter
# fail() -- report fail with optional detail lines
# assert_pwd() -- compare ${PWD:A} to ${1:A}
```

In `fail()`, use `"$@"` to accept multiple detail lines and loop over them:
`for line in "$@"; do print "    $line"; done`

- [ ] **Step 3: Rewrite existing tests to use the harness**

The single-match test becomes:

```zsh
it "cds to a unique top-level match"
cd $fixture
j alpha
assert_pwd $fixture/alpha
```

The error tests (missing arg, no match) become:

```zsh
it "errors with usage when called without an argument"
cd $fixture
if j 2>/dev/null; then fail "expected non-zero exit, got 0"; else ok; fi
```

- [ ] **Step 4: Add a summary block at the end of j.test.zsh**

```zsh
print ""
if (( tests_failed > 0 )); then
  print "  $tests_run tests, $tests_passed passed, $tests_failed FAILED"
  exit 1
else
  print "  $tests_run tests, all passed"
fi
```

- [ ] **Step 5: Run to confirm all three tests still pass**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `3 tests, all passed`

- [ ] **Step 6: Commit**

```bash
git add home-manager/modules/shells/tests/j.test.zsh
git commit -m "refactor: extract test harness from repetition"
```

---

### Task 7: Test + implement — case-insensitive match

**Files:**
- Modify: `home-manager/modules/shells/tests/j.test.zsh`
- Modify: `home-manager/modules/shells/files/functions/j`

**Interfaces:**
- Produces: `j ALPHA` finds `alpha/` regardless of query case

- [ ] **Step 1: Write the failing test using the harness**

```zsh
it "matches case-insensitively"
cd $fixture
j ALPHA
assert_pwd $fixture/alpha
```

- [ ] **Step 2: Run to confirm it fails**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `✗ matches case-insensitively`

- [ ] **Step 3: Add (#i) to the glob pattern in j**

In your `$matches` glob line, add the `(#i)` flag before the pattern:

```
matches=( $^roots/**/(#i)*${~query}*(/N) )
```

`(#i)` is an EXTENDED_GLOB flag that makes the rest of the pattern
case-insensitive. It is a glob flag, not a qualifier — it goes inside the
path, not at the end in `()`.

- [ ] **Step 4: Run to confirm all four tests pass**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `4 tests, all passed`

- [ ] **Step 5: Commit**

```bash
git add home-manager/modules/shells/tests/j.test.zsh \
        home-manager/modules/shells/files/functions/j
git commit -m "feat: j matches case-insensitively"
```

---

### Task 8: Test — nested directory match

**Files:**
- Modify: `home-manager/modules/shells/tests/j.test.zsh`

**Interfaces:**
- Consumes: `$fixture/nested/charlie` (already created in Task 1 fixture)
- Produces: test confirming `j charlie` reaches a deep basename match

- [ ] **Step 1: Write the test**

```zsh
it "matches a basename in a nested directory"
cd $fixture
j charlie
assert_pwd $fixture/nested/charlie
```

- [ ] **Step 2: Run — it likely already passes**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

If your `**` glob is already in `j`, this test probably passes with no
changes. If it fails, the issue is likely `$^roots` not distributing the
recursive glob correctly — re-read how `$^array` works.

- [ ] **Step 3: Commit**

```bash
git add home-manager/modules/shells/tests/j.test.zsh
git commit -m "test: nested directory match"
```

---

### Task 9: Multi-match — decide and document

**Files:**
- Modify: `home-manager/modules/shells/files/functions/j`

**Interfaces:**
- Produces: a documented decision on what `j` does when multiple directories
  match; the `*)` arm of the `case` is no longer missing

- [ ] **Step 1: Choose your approach**

Two valid options — pick one:

**Option A — Interactive select (untested):**
Add a `*)` arm using zsh's `select` builtin:

```zsh
  *)
    # Interactive picker — not covered by automated tests (requires TTY).
    local pick
    select pick in $matches; do
      [[ -n $pick ]] && { cd -- $pick; break }
    done
    ;;
```

`select` prints a numbered menu to stderr, reads a number from stdin, and
sets `$pick` to the chosen element. It loops until you `break`.

**Option B — Separate matching from selection:**
Refactor `j` so a helper function returns `$matches` and `j` calls it,
then decides what to do. The helper is testable; the selection UI is not.
Add a test for the helper. More work, but teaches single-responsibility.

- [ ] **Step 2: Implement your chosen approach**

Write the `*)` arm (and any helper, if Option B).

- [ ] **Step 3: Run all tests to confirm nothing regressed**

```bash
zsh home-manager/modules/shells/tests/j.test.zsh
```

Expected: `5 tests, all passed` (or however many you have)

- [ ] **Step 4: Commit**

```bash
git add home-manager/modules/shells/files/functions/j \
        home-manager/modules/shells/tests/j.test.zsh
git commit -m "feat: j multi-match picker"
```

---

## Done Criteria

- [ ] `zsh home-manager/modules/shells/tests/j.test.zsh` exits 0
- [ ] You can explain every line in both files without looking anything up
- [ ] No line in either file was written by anyone other than you
- [ ] The harness was extracted because you felt the repetition, not copied
