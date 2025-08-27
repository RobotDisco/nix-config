{ pkgs, ... }:

{
  services.gvfs.enable = true;

  environment = {
    shellAliases = {
      rm = "trash";
    };

    systemPackages = [ pkgs.trash-cli ];
  };
}
