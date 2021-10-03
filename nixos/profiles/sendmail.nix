{ isPortable ? false, ... }:

{
  services.postfix = {
    enable = true;
    rootAlias = "gdcosta@gmail.com";
    relayHost = "out.teksavvy.com";
    relayPort = 587;
  };
}
