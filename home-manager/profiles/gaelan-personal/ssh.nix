{
  programs.ssh = {
    enable = true;
    compression = true;
    # Don't forward by default, it is insecure
    # Prefer proxyjumping if you can
    forwardAgent = false;
  };
}
