# Personal zsh configuration, sourced from ~/.config/zsh/.zshrc via the
# wiring in home-manager/modules/shells/default.nix.
#
# Edit this file directly to iterate: `source ~/.config/zsh/rc.zsh` in any
# running shell applies changes immediately, no rebuild required.


# === Navigation options ==================================================
# AUTO_CD: type a directory name (no `cd`) to enter it.
# CDABLE_VARS: `cd varname` works when $varname holds a path.
# AUTO_PUSHD + dedup + silent: every cd builds a stack (`dirs -v`,
#   `cd -<TAB>` to navigate).
# EXTENDED_GLOB: enables ^, ~, # operators and glob qualifiers like
#   *(/N[1]) — required by j() for recursive-glob matching.
setopt AUTO_CD CDABLE_VARS AUTO_PUSHD PUSHD_IGNORE_DUPS PUSHD_SILENT
setopt EXTENDED_GLOB


# === Completion matching =================================================
# matcher-list is tried in order, falling through when no candidate
# matches the current strategy:
#   ''                              exact prefix
#   'm:{a-zA-Z}={A-Za-z}'           case-insensitive
#   'r:|[._-]=* r:|=*'              match after . _ or - boundaries
#                                   (so `hm-mod<TAB>` -> home-manager/modules)
#   'l:|=* r:|=*'                   substring anywhere
zstyle ':completion:*' matcher-list \
  '' \
  'm:{a-zA-Z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'

# Arrow-key menu picker once more than one candidate remains.
zstyle ':completion:*' menu select
# cdr always opens the picker — its purpose is interactive selection.
zstyle ':completion:*:*:cdr:*:*' menu selection


# === Workspace navigation ================================================
# Bare-name cd searches each cdpath entry in order; earlier entries win on
# ambiguity. Keep this list short — too many roots makes ambiguity the
# norm and slows tab completion.
cdpath=(
  $HOME/code
  $HOME/code/nix-config/home-manager
  $HOME/code/nix-config/home-manager/modules
)

# hash -d makes ~name expand to a path AND renders cwd that way in
# prompts. Add bookmarks as patterns settle; remove when they go stale.
hash -d nix=$HOME/code/nix-config
hash -d hm=$HOME/code/nix-config/home-manager
hash -d hmm=$HOME/code/nix-config/home-manager/modules

# Recent-directory tracker (zsh-native, no plugin). Every cd appends to
# the recents file; `cdr` lists with menu selection; the j function can
# read from here for recency-aware matching.
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 50
zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-file \
  "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/chpwd-recent-dirs"


# === Prompt ==============================================================
# Required for the $(...) and ${...} expansions in PS1/RPS1 below.
setopt PROMPT_SUBST

# vcs_info populates ${vcs_info_msg_0_} with git branch info on each
# prompt. Registered via add-zsh-hook (not `precmd() { ... }`) so other
# hooks — direnv, VTE title-setter, future additions — compose cleanly.
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
add-zsh-hook precmd vcs_info

# %h   history number for referencing this command later
# %2~  cwd, max 2 trailing components, ~-collapsed (named dirs included)
export PS1='!%h %2~ $ '

# Right prompt: kube context, vcs status, ✓ / ✗ on last exit.
export RPS1=$'$(kubectl config current-context 2>/dev/null) ${vcs_info_msg_0_} %0(?,\U2713,\U2717)'
