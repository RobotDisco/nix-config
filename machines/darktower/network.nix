{
  networking = {
    # Create a private subnet for all my home network services.
    bridges.br20.interfaces = [ "vlan20" ];
    bridges.br50.interfaces = [ "vlan50" ];

    # Use control plane interface for outbound traffic by default.
    defaultGateway = {
      address = "192.168.10.1";
      interface = "eno1";
    };

    interfaces = {
      # Configure our admin interface (don't route external ingress
      # traffic through here as a form of network security.
      eno1 = {
        useDHCP = false;
        ipv4.addresses = [
          {
            address = "192.168.10.3";
            prefixLength = 24;
          }
        ];
      };
    };

    # Use control plane for nameserver resolution by default
    nameservers = [ "192.168.10.1" ];

    # We don't use DHCP on this server
    useDHCP = false;

    vlans = {
      # Specify the VLAN for home services
      vlan20 = {
        id = 20;
        interface = "enp6s0f1";
      };
      # Specify the VLAN for cloud services
      vlan50 = {
        id = 50;
        interface = "enp6s0f1";
      };
    };
  };
}
