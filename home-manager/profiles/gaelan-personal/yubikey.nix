{
  config,
  pkgs,
  ...
}:

{
  home.packages = [ pkgs.yubioath-flutter ];

  # In home-manager, the .path attribute relies on the nix config
  # eventually resolving environment variables and command subshells.
  #
  # Apparently using mkOutOfStoreSymlink doesn't work, because it doesn't
  # evaluate the .path attribute including resolutions.
  #
  # So instead, use an activation hook.
  #home.activation.linkU2fKeys = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #  run ln -sf $VERBOSE_ARG "${config.age.secrets.u2f_keys.path}" "${config.home.homeDirectory}/.config/Yubico/u2f_keys"
  # '';

  # Or maybe the answer is just to decrypt directly to where I want the file,
  # instead of relying on the .path default which assumes variable expansion...
  age.secrets.u2f_keys.path = "${config.xdg.configHome}/Yubico/u2f_keys";
}
