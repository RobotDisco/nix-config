{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:

let
  cfg = config.robot-disco.claude-code;

  # Enabled on every machine that turns this module on. Profile-specific
  # plugins layer on top via robot-disco.claude-code.settings.enabledPlugins;
  # a profile can also disable an entry here by setting it to false.
  commonPlugins = {
    "claude-code-setup@claude-plugins-official" = true;
    "claude-md-management@claude-plugins-official" = true;
    "code-review@claude-plugins-official" = true;
    "code-simplifier@claude-plugins-official" = true;
    "commit-commands@claude-plugins-official" = true;
    "desktop-commander@claude-plugins-official" = true;
    "explanatory-output-style@claude-plugins-official" = true;
    "learning-output-style@claude-plugins-official" = true;
    "skill-creator@claude-plugins-official" = true;
    "superpowers@claude-plugins-official" = true;
  };
in

{
  options.robot-disco.claude-code = {
    enable = lib.mkEnableOption "Enable Claude Code";

    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = ''
        Extra settings layered on top of the common baseline and passed
        through to programs.claude-code.settings.
      '';
    };

    mcpServers = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "MCP servers passed through to programs.claude-code.mcpServers.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      # Claude constantly wants to use python, so install it
      pkgs.python3
    ];

    programs.claude-code = {
      enable = true;

      # Always use the latest
      package = pkgs-unstable.claude-code;

      inherit (cfg) mcpServers;

      settings = cfg.settings // {
        enabledPlugins = commonPlugins // (cfg.settings.enabledPlugins or { });
      };
    };
  };
}
