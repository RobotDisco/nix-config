# j TDD Rewrite Design

**Date:** 2026-06-28
**Goal:** Rewrite the `j` directory-jumping function from scratch using TDD,
learning zsh by doing rather than reading someone else's code.

## Motivation

The existing `j` and `j.test.zsh` were not written by the user. The shell
framework goal is to own every line — no black boxes. Rewriting `j` from
blank slate using TDD is the entry point for learning zsh-specific features
(autoload, glob qualifiers, ZLE, etc.) that go beyond bash-style scripting.

## Starting State

Delete both existing files:

```
home-manager/modules/shells/files/functions/j
home-manager/modules/shells/tests/j.test.zsh
```

Create them fresh in the same locations. The test file is written first;
the implementation follows to make tests pass.

## File Layout

```
home-manager/modules/shells/
  files/
    functions/
      j                    ← autoloaded implementation (written last)
  tests/
    j.test.zsh             ← test file + inline harness (written first)
```

The test file sources nothing external. It:
1. Creates a fixture tree with `mktemp -d`
2. Sets `J_ROOTS=( $fixture )` to point `j` at the fixture
3. Autoloads `j` from the in-repo `files/functions/` directory (not the
   installed copy) via `fpath=( $functions_dir $fpath ); autoload -Uz j`
4. Runs assertions and reports results

## TDD Learning Arc

### Phase 1 — First test, first implementation

Write one test using raw `if/then/print`:

- Create `$fixture/alpha`
- Call `j alpha`
- Check that `$PWD` equals `$fixture/alpha`

Then create `j` and write the minimum code to pass that one test.
Concepts encountered: `autoload`, `emulate -L zsh`, `local`, array syntax,
glob qualifiers `(/N)`.

### Phase 2 — Grow tests, feel the repetition

Add cases one at a time:

- Missing argument → non-zero exit with usage message on stderr
- No match → non-zero exit with error on stderr
- Case-insensitive match (`J ALPHA` finds `alpha/`)
- Nested directory match (query matches a basename deep in the tree)

Each new test is another raw `if/then` block. By the third or fourth, the
repetition becomes obvious.

### Phase 3 — Extract the harness

When the copy-paste bothers you, pull helper functions out inline in
`j.test.zsh`:

```zsh
it()   { current_test=$1 }
ok()   { print "  ✓ $current_test" }
fail() { print "  ✗ $current_test"; print "    $*" }
```

This is the moment you understand why test harnesses exist — you invented
the need. Concepts encountered: `local -i`, `(( ))` arithmetic,
`print -u2` for stderr.

The harness stays inside `j.test.zsh` for now. No separate file until a
second module needs to reuse it.

### Phase 4 — Edge cases and the picker

The multi-match `select` case is interactive and hard to test directly.
Options:

- Document it as a known limitation (skip testing it)
- Separate the matching logic from the `cd`/`select` call so matching
  can be tested independently — teaches single-responsibility in shell

This decision is left to the implementer; either is valid.

## Running Tests

```sh
zsh home-manager/modules/shells/tests/j.test.zsh
# or via just:
just test-zsh
```

## Done Criteria

- `j.test.zsh` written entirely by the user, harness extracted from
  observed repetition
- `j` implemented entirely by the user, passing all tests
- `just test-zsh` (or direct invocation) exits green
- User can explain every line in both files

## Out of Scope

- Multi-match picker testing (document as known limitation)
- Recency weighting
- Performance pruning for large trees

## Path Forward

The pattern established here — autoloaded function, fixture-based test
file, harness extracted from repetition — is the template for every
subsequent shell module. The next function starts by copying this structure.
