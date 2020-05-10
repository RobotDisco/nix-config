with (import <nixpkgs> {});
{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    userPackages = buildEnv {
      inherit ((import <nixpkgs/nixos> {}).config.system.path)
        pathsToLink ignoreCollisions postBuild;
      extraOutputsToInstall = [ "man" ];
      name = "user-packages";
      paths = [
        awscli
        # chefdk
        clojure
        dosbox
        file
        fzf
        google-cloud-sdk
        pavucontrol
        seafile-shared
        inotify-tools
        libsearpc
        timidity
        # Fluid3
        pmidi
        transmission
        vlc
        # wine-wow
        chromium
        firefox
        git
        gnumake
        go
        sbcl
        emacs
        bitwarden
        bitwarden-cli
        yubioath-desktop
        signal-desktop
        obs-studio
        dwarf-fortress-packages.dwarf-fortress-full
        unzip
        zsh
        keychain
        xmobar
        scrot
        steam
        ripgrep
        fasd
        networkmanager_l2tp
      ];
    };
  };
}
