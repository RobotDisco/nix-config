# Requires: nix develop (provides EMACS_NOX, SYSTEM, and all tools)

emacs_dev_dir := env_var_or_default("XDG_RUNTIME_DIR", "/tmp") \
    + "/emacs-dev"
emacs_init_org := justfile_directory() / "packages/emacs/init.org"

# Tangle init.org to a temp dir for isolated testing
tangle:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p "{{emacs_dev_dir}}"
    cp "{{emacs_init_org}}" "{{emacs_dev_dir}}/"
    cd "{{emacs_dev_dir}}" && "$EMACS_NOX" \
        --batch --load org init.org \
        --funcall org-babel-tangle
    echo "Files:"
    ls "{{emacs_dev_dir}}"/*.el 2>/dev/null \
        || echo "  (none produced)"

# Check tangled init.el for syntax errors (paren balance etc.)
check-emacs: tangle
    "$EMACS_NOX" --batch \
        --eval "(progn (find-file \"{{emacs_dev_dir}}/init.el\") \
                       (check-parens) \
                       (message \"Syntax OK\"))"

# Tangle init.org and launch emacs with the result (elisp changes)
emacs-dev *args: tangle
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -f "{{emacs_dev_dir}}/init.el" ]; then
        echo "No tangled config found. Run: just tangle"
        exit 1
    fi
    echo "Starting emacs with dev config from {{emacs_dev_dir}}"
    echo "(~/.config/emacs/ is not affected)"
    exec emacs --init-directory "{{emacs_dev_dir}}" \
        --eval "(load-theme 'misterioso t)" {{args}}

# Tangle and launch a freshly built emacs derivation (new packages)
emacs-dev-package *args: tangle
    #!/usr/bin/env bash
    set -euo pipefail
    result=$(nix build ".#packages.$SYSTEM.emacs" --no-link --print-out-paths)
    exec "$result/bin/emacs" --init-directory "{{emacs_dev_dir}}" \
        --eval "(load-theme 'misterioso t)" {{args}}

# Build the emacs derivation standalone (verify package resolution)
build-emacs:
    nix build ".#packages.$SYSTEM.emacs" --no-link

# Build home-manager config without activating (verify it evaluates)
build-home host:
    nix build '.#homeConfigurations."gaelan@{{host}}".activationPackage' \
        --no-link

# Build home-manager config and link ./result for inspection
inspect-home host:
    nix build '.#homeConfigurations."gaelan@{{host}}".activationPackage'
    @echo "Inspect with: ls result/home-path/bin/"

# Apply home-manager config (previous generation available for rollback)
switch-home host:
    home-manager switch --flake '.#gaelan@{{host}}'
