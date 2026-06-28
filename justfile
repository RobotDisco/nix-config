# Requires: nix develop (provides EMACS_NOX, SYSTEM, and all tools)

emacs_dev_dir := env_var_or_default("XDG_RUNTIME_DIR", "/tmp") \
    + "/emacs-dev"
emacs_init_org := justfile_directory() / "packages/emacs/init.org"

# --- System builds ---

# Build arrakis (NixOS laptop)
build-arrakis:
    nix build \
        .#nixosConfigurations.arrakis.config.system.build.toplevel \
        --no-link

# Build darktower (NixOS server)
build-darktower:
    nix build \
        .#nixosConfigurations.darktower.config.system.build.toplevel \
        --no-link

# Build fountain-of-ahmed-iii (macOS)
build-fountain-of-ahmed-iii:
    nix build \
        .#darwinConfigurations.fountain-of-ahmed-iii.system \
        --no-link

# Build all system configs
build-all: build-arrakis build-darktower
    #!/usr/bin/env bash
    set -euo pipefail
    if [ "$SYSTEM" = "aarch64-darwin" ]; then
        just build-fountain-of-ahmed-iii
    fi

# --- System apply ---

# Test config switch without permanent changes
test-switch:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ "$SYSTEM" = "aarch64-darwin" ]; then
        # darwin-rebuild has no test subcommand; switch and rollback if needed
        echo "Note: darwin has no test mode — switching now." \
             "Run 'darwin-rebuild --rollback' to revert."
        sudo darwin-rebuild switch --flake .
    else
        sudo nixos-rebuild test --flake .
    fi

# Apply system config permanently (prompts for confirmation)
apply:
    #!/usr/bin/env bash
    set -euo pipefail
    read -p "Are you sure you want to apply? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        if [ "$SYSTEM" = "aarch64-darwin" ]; then
            sudo darwin-rebuild switch --flake .
        else
            sudo nixos-rebuild switch --flake .
        fi
    else
        echo "Apply cancelled"
        exit 1
    fi

# Deploy to darktower via SSH
apply-darktower:
    nixos-rebuild switch --flake .#darktower \
        --target-host 192.168.10.3 \
        --sudo --ask-sudo-password

# --- Home-manager ---

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

# Apply nix-darwin config (previous generation available for rollback)
switch-darwin host:
    darwin-rebuild switch --flake '.#{{host}}'

# --- Emacs ---

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
    result=$(nix build ".#emacs" --no-link --print-out-paths)
    exec "$result/bin/emacs" --init-directory "{{emacs_dev_dir}}" \
        --eval "(load-theme 'misterioso t)" {{args}}

# Build the emacs derivation standalone (verify package resolution)
build-emacs:
    nix build ".#emacs" --no-link

# --- Maintenance ---

# Format all Nix files
fmt:
    #!/usr/bin/env bash
    set -euo pipefail
    find . -name "*.nix" ! -name "hardware-configuration.nix" \
        -exec nixfmt {} \;

# Run all linting checks
lint:
    nix build ".#checks.$SYSTEM.lint-check" --no-link

# Run flake checks
check:
    nix flake check

# Update and commit flake inputs
update:
    nix flake update --commit-lock-file

# Run zsh unit tests
test-zsh:
	zsh home-manager/modules/shells/tests/j.test.zsh
