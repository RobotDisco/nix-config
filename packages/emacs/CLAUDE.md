# Emacs Configuration

`init.org` is a literate config — **never edit `init.el` or `early-init.el`
directly**. Both are tangled outputs. All changes go in `init.org`.

**After making significant changes to keybindings, packages, or
workflows, update `packages/emacs/README.org`** — it is the user-facing
manual covering all configured workflows and key bindings. Keep it in
sync so it remains a reliable quick-reference.

## Workflow

```
just check-emacs        # tangle + validate paren balance (fast)
just emacs-dev          # tangle + launch sandboxed Emacs (elisp iteration)
just emacs-dev-package  # full Nix rebuild — use when adding new packages
just tangle             # tangle only
```

`just emacs-dev` uses an isolated config dir and does not affect
`~/.config/emacs/`. Apply to the real config via `just switch-home`.

## Architecture

### Task management (org)

- `~/Documents/gtd/inbox.org` — all captures land here
- `~/Documents/gtd/personal.org` — habits, someday, self-directed tasks
- `~/Documents/brain/notes/projects/` — active project notes (ZK nodes)
- `~/Documents/gtd/jira/CLOUD.org` — work tasks (org-jira, read-only)
- `~/Documents/gtd/gcal/` — calendar events (org-gcal)

All task files are in `org-agenda-files`. JIRA is canonical for
work sprint/interrupt tasks. Self-directed work tasks live in
`personal.org` tagged `:@work:`.

### Agenda views

- `C-c a .` — unified: DOING (max 3) / 3-day agenda / On Deck
- `C-c a W` — work view (JIRA + work week, filtered `:@work:`)
- `C-c a P` — personal view (personal week, filtered `:-@work:`)
- `C-c a i` — inbox triage

### Capture keys

- `C-c c t` — task → inbox
- `C-c c s` — scheduled task → inbox (date prompt)
- `C-c c e` — work note → inbox (`:@work:`)
- `C-c c a` — action from current note → inbox (with link)
- `C-c n p` — new project note (org-roam, straight to `j` template)

### TODO keyword sequence

```
TODO → GROOM → NEXT → DOING → WAIT → DONE/HALTED
PROJECT → ACTIVE → PAUSED → DONE/HALTED
```

WIP limit: max 3 DOING at any time. NEXT/DOING replaces priority
flags. GROOM exists to mirror the JIRA grooming workflow.

### org-roam

- `org-roam-directory`: `~/Documents/brain/notes`
  - Zettelkasten notes: `~/Documents/brain/notes/` (root)
  - Daily journal: `~/Documents/brain/notes/journal/` subdirectory
- Project notes: `projects/` subdirectory (also in agenda-files)
- `C-c n r` — random node review (excludes journal and literature)
- `C-c j j` — bullet journal entry (today)
- `C-c j T` → `w` — weekly reflection template (file-based)

### Daily schedule

Work days: Tuesday–Friday, 8am–5pm. Monday off (weekly review day).

| Time  | What |
|-------|------|
| 7:00am | Wake + morning pills (phone alarm) |
| 9:30am | Physio #1 (phone alarm) |
| 10:00am | Weekly review — Mondays only (org-habit) |
| 12:00pm | Lunch |
| 12:20pm | Cardio / treadmill (org-habit) |
| 1:30pm | Physio #2 (phone alarm) |
| 3:30pm | Physio #3 (phone alarm) |
| 4:50pm | Evening capture sweep (org-habit + phone alarm) |
| 5:30pm | Physio #4 (phone alarm) |
| 7:30pm | Physio #5 + evening pills (phone alarm) |
| 9:00am | Kingshot bear traps — EDT (phone alarm, 30 min block in gcal) |
| 6:30pm | Kingshot bear traps — EDT (phone alarm, 30 min block in gcal) |
| 8:00pm | Kingshot reset — EDT (phone alarm) |
| 10:30pm | Wind down + bedtime pills (phone alarm) |
| 11:30pm | Lights out (phone alarm) |

Physio alternates with cardio every other day (`++2d` in org-habit).
Exercise at lunch is a pragmatic default; morning exercise (Monday) is
neurologically superior when no 8am deadline exists.

Habits tracked in org (streak graph):
- Physio, Cardio, 5x Cobra Pose, Evening capture sweep,
  Weekly Review, Slept by midnight

### Completion & navigation stack

The minad stack replaces most default Emacs completion/search UIs:

- **vertico** — minibuffer completion UI
- **orderless** — fuzzy/multi-token matching for all completion
- **marginalia** — annotations in minibuffer (file sizes, doc strings, etc.)
- **consult** — structured sources and live search
- **embark** — context-sensitive actions on completion candidates
- **corfu** — in-buffer completion popup (auto-triggers after 0.5s)
- **cape** — completion sources for corfu: dabbrev (seen words) + file paths

Key remapped bindings (differ from Emacs defaults):

| Key | Command |
|-----|---------|
| `C-x b` | `consult-buffer` |
| `C-x f` | `consult-recent-file` |
| `M-y` | `consult-yank-pop` |
| `M-g g` | `consult-goto-line` |
| `M-g o` | `consult-outline` |
| `M-g i` | `consult-imenu` |
| `M-s l` | `consult-line` |
| `M-s r` | `consult-ripgrep` |
| `C-.` | `embark-act` |
| `C-;` | `embark-dwim` |
| `C-'` | `avy-goto-char-timer` |
| `M-g j` | `avy-goto-line` |
| `C-c e s` | `consult-eglot-symbols` (in eglot buffers) |
| `C-c [` / `C-c ]` | `winner-undo` / `winner-redo` |

`embark-export` during any consult search exports results to a
persistent grep-mode/occur-mode buffer. `wgrep` (`C-c C-p` in that
buffer) makes it editable; `C-c C-c` commits changes across all files.

### Format on save (apheleia)

Apheleia formats on save using absolute Nix store paths. Current
formatter → mode assignments:

| Mode | Formatter |
|------|-----------|
| `nix-mode` | `nixfmt` |
| `clojure-mode`, `clojurec-mode`, `clojurescript-mode` | `zprint` |
| `yaml-mode`, `json-mode` | `prettier` |
| `terraform-mode` | `terraform fmt -` |

### External binaries

Home-manager's `services.emacs` module launches Emacs via a login
shell, so the daemon has the user's full `$PATH` (`home.packages`
included) — bare command names work.

### Known design decisions

- `j` capture template targets `projects/` not the main ZK root,
  so project notes stay out of the 2390+ general note pool while
  remaining proper org-roam nodes with IDs and backlinks.
- `w` weekly template uses a file reference
  (`~/Documents/brain/templates/weekly.org`) not an inline string,
  so it can be edited without retangling.
- `:@work:` is the context tag for work tasks (GTD convention).
  Agenda filters use `+@work` / `-@work`.
- `blocked` tag (key: `b`) is used on JIRA tasks instead of `WAIT`
  state, since JIRA uses a flag rather than a status for blocked work.
- `rg.el` is intentionally absent — `consult-ripgrep` (`M-s r`) covers
  the same use case with better consult integration.
