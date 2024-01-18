{ lib, buildPythonApplication, fetchPypi, beautifulsoup4, boto3, click
, configparser, pip, requests, setuptools, validators }:

buildPythonApplication rec {
  pname = "okta-awscli";
  version = "0.5.4";

  src = fetchPypi {
    inherit pname version;
    sha256 = "UJkho43txvoUJPBuW7lKW7NZRjkSxFIYq99glbOqyCE=";
  };

  # Need this because okta-awscli depends on a shadow package for some reason
  # https://github.com/okta-awscli/okta-awscli/issues/141
  preConfigure = ''
    substituteInPlace setup.py --replace "bs4" "beautifulsoup4"
  '';

  nativeBuildInputs = [ pip setuptools ];

  propagatedBuildInputs =
    [ beautifulsoup4 boto3 click configparser requests validators ];

  meta = {
    description = "Provides a wrapper for Okta authentication to awscli";
    homepage = "https://github.com/okta-awscli/okta-awscli";
    maintainers = [{
      email = "gdcosta@gmail.com";
      github = "RobotDisco";
      githubId = "487847";
      name = "Gaelan D'costa";
    }];
    license = lib.licenses.asl20;
  };
}
