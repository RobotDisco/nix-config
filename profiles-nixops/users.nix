{
  users.users.gaelan = {
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    openssh.authorizedKeys.keyFiles = [
      ./gaelan-yubikey.pub
    ];
  };
}
