{
  lib,
  fetchFromGitHub,
  rustPlatform,
  notmuch,
}:

rustPlatform.buildRustPackage {
  pname = "mujmap";
  version = "RobotDisco-15f99fb8aae870ab85b617bc4c07e5842b964f00";

  # Tracking Lyndeno's fork: upstream (elizagamedev) is abandoned at
  # v0.2.0 and missing the JMAP `Core` capability declaration that
  # current Fastmail requires (returns 403 without it).
  src = fetchFromGitHub {
    owner = "RobotDisco";
    repo = "mujmap";
    rev = "15f99fb8aae870ab85b617bc4c07e5842b964f00";
    sha256 = "sha256-0sYf6ijbM2NP/6TRdFVFsrsFdHlaMi/HE0CEkoNkDDM=";
  };

  cargoHash = "sha256-x3ef5RajIiZfImxwue9bTUmayTWu1PA08/bHiVH5ee4=";

  buildInputs = [
    notmuch
  ];

  meta = {
    description = "JMAP integration for notmuch mail";
    homepage = "https://github.com/elizagamedev/mujmap/";
    license = lib.licenses.gpl3Plus;
    mainProgram = "mujmap";
  };
}
