{
  containers = {
    reverseproxy = {
      autoStart = true;
      privateNetwork = true;
      hostBridge = "br50";
      localAddress = "192.168.50.99/24";
      config = {
        system.stateVersion = "21.05";

        networking = {
          defaultGateway = "192.168.50.1";
          firewall = {
            allowedTCPPorts = [
              80
              443
            ];
          };
          nameservers = [ "192.168.50.1" ];
        };

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
    interfaces.br50.allowedTCPPorts = [
      80
      443
    ];
  };
}
