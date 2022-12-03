{ pkgs }:

pkgs.python3Packages.buildPythonPackage rec {
  name = "okta-awscli";
  src = pkgs.fetchPypi {
    inherit name;
    version = "0.5.4";
  };
}
