{ pkgs, ... }:
let
  gtkGreetStyleSheet = pkgs.writeText "gtkgreet.css" ''
    window {
      background-image: url("file://${../../../../backgrounds/yotsugi_hat.jpg}");
      background-size: cover;
      background-position: center;
    }

    box {
      /*
       * Rebecca Purple - https://medium.com/@valgaze/the-hidden-purple-memorial-in-your-web-browser-7d84813bb416
       * This mostly translucent box is being added to give the text readability
       */
      background-color: rgba(66,33,99,0.30);

      color: gold;
    }
  '';
  cmpstr = pkgs.hyprland;
  cmpstrCfg = pkgs.writeText "greet-hypr-config" ''
    # Disable power-hungry animations
    decoration {
      blur {
        enabled = false
      }
      shadow {
        enabled = false
      }
    }

    misc {
    # Disable hyprland vanity logo and text.
      disable_hyprland_logo = true
      disable_splash_rendering = true
    # Lower frame rate
      vfr = true
    }

    # We don't need xwayland for our login screen
    xwayland {
      enabled = false
    }

    # `-l` activates layer-shell mode. Notice that `swaymsg exit` will run after gtkgreet.
    exec-once = ${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -s ${gtkGreetStyleSheet}; ${cmpstr}/bin/hyprctl exit
  '';
in
{
  services.greetd = {
    enable = true;
    settings = {
      default_session.command = "${cmpstr}/bin/Hyprland --config ${cmpstrCfg}";
      initial_session = {
        user = "gaelan";
        command = "Hyprland";
      };
    };
  };

  environment.etc."greetd/environments".text = ''
    Hyprland
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
