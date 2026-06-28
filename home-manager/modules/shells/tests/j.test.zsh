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

# TODO(human): write your first test here
