{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.gaelan.linux-laptop;

in {
  options.gaelan.linux-laptop.enable = mkEnableOption "Enables linux things.";

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # chefdk
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

    systemd.user.services."seafile-cli" = {
      after = [ "network.target" ];
      enable = true;
      description = "Seafile CLI Client";
      wantedBy = [ "default.target" ];
      path = [ pkgs.seafile-shared ];
      serviceConfig.Type = "oneshot";
      serviceConfig.ExecStart = "${pkgs.seafile-shared}/bin/seaf-cli start";
      serviceConfig.ExecStop = "${pkgs.seafile-shared}/bin/seaf-cli stop";
      serviceConfig.RemainAfterExit="yes";
    };
  };
}
