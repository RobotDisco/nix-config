{ ... }:

{
  home.file.gpg-conf = {
    source = ./gpg.conf;
    target = ".gnupg/gpg.conf";
  };
  home.file.gpg-agent-conf = {
    source = ./gpg-agent.conf;
    target = ".gnupg/gpg-agent.conf";
  };
}
