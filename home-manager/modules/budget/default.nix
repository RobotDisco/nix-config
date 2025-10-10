{ pkgs, ... }:

let
  inherit (pkgs) hledger;
in
{
  home.packages = [
    hledger
    (pkgs.writeShellApplication {
      name = "hledger-emacs.sh";
      runtimeInputs = [ hledger ];
      text = builtins.readFile ./hledger-emacs.sh;
    })
  ];
}
