# Work-related system config. This should go in a tulip-specific flake.
{
  # We need strongswan support for Tulip's VPN.
  networking.networkmanager.enableStrongSwan = true;
}
