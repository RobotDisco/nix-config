{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.gaelan.linux-laptop;

in {
  options.gaelan.linux-laptop.enable = mkEnableOption "Enables linux things.";

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # chefdk
      clojure
      dosbox
      file
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
      networkmanager_l2tp
    ];

    fonts.fontconfig.enable = true;
  };
}
