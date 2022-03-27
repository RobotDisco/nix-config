{ lib, nixpkgs, ... }:

{
    mkHost = {
        system,
        hostName,
        dhcpInterfaces,
        initrdAvailableModules,
        kernelModules,
        kernelParams,
        kernelPackages,
        cpuCores ? "auto",
        stateVersion ? "21.11",
        additionalModules
    }:

    let
        networkInterfaceCfg = listToAttrs (map
            (n: {
                name = n;
                value = { useDHCP = true; };
            })
            dhcpInterfaces);
        
    in

    lib.nixosSytem {

        modules = [
            nixpkgs.nixosModules.notDetected
            {
                networking.hostName = hostname;
                networking.interfaces = networkInterfaceCfg;
                networking.networkmanager.enable = true;
                networking.useDHCP = false;

                boot.initrd.availableKernelModules = initrdModules;
                boot.kernelModules = kernelModules;
                boot.kernelParams = kernelParams;
                boot.kernelPackages = kernelPackage;

                nix.MaxJobs = lib.mkDefault cpuCores;

                system.stateVersion = stateVersion;
            }
        ] ++ additionalModules;
    };
}