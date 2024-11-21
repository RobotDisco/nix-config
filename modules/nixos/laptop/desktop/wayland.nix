{ pkgs, ... }:
let
  gtkGreetStyleSheet = pkgs.writeText "gtkgreet.css" ''
    window {
      background-image: url("file://${../../../../backgrounds/yotsugi_hat.jpg}");
      background-size: cover;
      background-position: center;
    }
  '';
  swayConfig = pkgs.writeText "greet-sway-config" ''
    monitor * enable
    # `-l` activates layer-shell mode. Notice that `swaymsg exit` will run after gtkgreet.
    exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -s ${gtkGreetStyleSheet}; ${pkgs.sway}/bin/swaymsg exit"
    bindsym Mod4+shift+e exec ${pkgs.sway}/bin/swaynag \
      -t warning \
      -m 'Perform an action:' \
      -b 'Power Down' 'systemctl poweroff' \
      -b 'Reboot' 'systemctl reboot'
  '';
in
{
  services.greetd = {
    enable = true;
    settings.default_session.command = "${pkgs.sway}/bin/sway --config ${swayConfig}";
  };

  environment.etc."greetd/environments".text = ''
    hyprland
    zsh
  '';

  # Compositors I'm actively using
  programs.hyprland.enable = true;

  # A bunch of packages that are likely better managed via home-manager.
  environment.systemPackages = with pkgs; [
    # Handle monitor brightness
    pkgs.brightnessctl
    # Handle wireless/bluetooth toggling (rfkill)
    pkgs.util-linux
    # Handle volume management
    pkgs.wireplumber
    # Pretty application launcher
    pkgs.wofi
    # TODO Decide on one of these file managers
    pkgs.pcmanfm
    pkgs.xfce.thunar
    # TODO Decide on one of these fancy terminals
    pkgs.kitty
    pkgs.alacritty
    # Handle notifications
    pkgs.mako
    # Status Bar
    pkgs.waybar
    # Tools clipboard managers need.
    pkgs.wl-clipboard
    # Clip managers to try out
    pkgs.cliphist
    pkgs.clipse
    # Session management. Handle idle sessions, locking sessions
    pkgs.hypridle
    pkgs.hyprlock
    # Background management
    pkgs.hyprpaper
    # Colour picker
    pkgs.hyprpicker
  ];
  # Hyprland's manager for idle inactivity
  services.hypridle.enable = true;
  # Hyprland handle locking the session
  programs.hyprlock.enable = true;
}
