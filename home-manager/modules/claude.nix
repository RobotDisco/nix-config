{
  config,
  lib,
  pkgs-unstable,
  ...
}:

let
  cfg = config.robot-disco.claude-code;
in

{
  options.robot-disco.claude-code = {
    enable = lib.mkEnableOption "Enable Claude Code";

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Settings passed through to programs.claude-code.settings.";
    };
  };

  config.programs.claude-code = {
    inherit (cfg) enable settings;

    # Always use the latest
    package = pkgs-unstable.claude-code;
  };
}
