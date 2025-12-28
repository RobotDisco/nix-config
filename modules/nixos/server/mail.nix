{
  services.postfix = {
    enable = true;
    rootAlias = "gdcosta@gmail.com";
    settings.main.relayhost = [ "out.teksavvy.com:587" ];
  };
}
