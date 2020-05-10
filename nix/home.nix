{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";

  nixpkgs.config.allowUnfree = true;
  
  home.packages = with pkgs; [
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
}
