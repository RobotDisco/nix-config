{
  containers = {
    reverseproxy = {
      autoStart = true;
      config = {
        system.stateVersion = "21.05";

        security.acme = {
          acceptTerms = true;
          defaults = {
            # credentialFiles = {
            #   EASYDNS_KEY = "";
            #   EASYDNS_TOKEN = "";
            # };
            # dnsProvider = "easydns";
            email = "gdcosta+letsencrypt@gmail.com";
          };
        };

        services.nginx = {
          enable = true;
          defaultListenAddresses = [ "192.168.50.99" ];
          recommendedBrotliSettings = true;
          recommendedOptimisation = true;
          recommendedTlsSettings = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          recommendedZstdSettings = true;
        };
      };
    };
  };

  networking.firewall = {
    checkReversePath = "loose";

    interfaces.vlan50.allowedTCPPorts = [
      80
      443
    ];
  };
}
