{
  # Don't allow anyone except those in the admin group to
  # perform a sudo.
  security.sudo.execWheelOnly = true;

  # Let sudo authenticate against a forwarded SSH agent (i.e. my
  # YubiKey on the machine I'm sshing from) instead of a password.
  # Falls back to the password when no agent is forwarded.
  security.pam = {
    sshAgentAuth = {
      enable = true;
      # Only trust root-owned keys (from users.users.*.openssh), never
      # ~/.ssh/authorized_keys, which the user could write to themselves.
      authorizedKeysFiles = [ "/etc/ssh/authorized_keys.d/%u" ];
    };
    services.sudo.sshAgentAuth = true;
  };
}
