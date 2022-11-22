{ pkgs, ... }:

{
  xdg.enable = true;

  programs.ssh = {
    enable = true;
    compression = true;
    # Don't forward by default, it is insecure
    # Prefer proxyjumping if you can
    forwardAgent = false;
  };

  programs.zsh = {
    # Since I use zsh, make sure home-manager sets it up.
    enable = true;
  };

  home.keyboard = {
    layout = "us";
    options = [ "ctrl:nocaps" ];
  };

  programs.home-manager.enable = true;

  home.packages = with pkgs; [ brave bitwarden calibre discord signal-desktop slack ];
}
