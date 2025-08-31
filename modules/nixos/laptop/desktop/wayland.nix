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

    # Remove caps-lock
    input = {
      kb_layout = "us";
      kb_options = "ctrl:nocaps";
    };

    # `-l` activates layer-shell mode. Notice that `hyprctl exit` will run after gtkgreet exits.
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

  # Hyprland's manager for idle inactivity
  services.hypridle.enable = true;
  # Hyprland handle locking the session
  programs.hyprlock.enable = true;
}
