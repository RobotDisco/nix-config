{
  lib,
  fetchFromGitHub,
  rustPlatform,
  notmuch,
}:

rustPlatform.buildRustPackage {
  pname = "mujmap";
  version = "Lyndeno-883e5c8";

  # Tracking Lyndeno's fork: upstream (elizagamedev) is abandoned at
  # v0.2.0 and missing the JMAP `Core` capability declaration that
  # current Fastmail requires (returns 403 without it).
  src = fetchFromGitHub {
    owner = "Lyndeno";
    repo = "mujmap";
    rev = "883e5c8cb5c2ea2e2b3ebfe430d7cb0adfa38d5d";
    sha256 = "sha256-2v7lnoQ68DaorMOubnE++IvyYYdAqyMfKuA6cvmqAsI=";
  };

  cargoHash = "sha256-/c4vOql13au/mcOP8kICgRg+6ZAJjpLKcKikVbnXYws=";

  # Fix all-mail query when notmuch DB path == maildir path
  # (per-account-DB layout). See patch header for details.
  patches = [ ./mujmap-fix-empty-path-query.patch ];

  buildInputs = [
    notmuch
  ];

  meta = {
    description = "JMAP integration for notmuch mail";
    homepage = "https://github.com/elizagamedev/mujmap/";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ RobotDisco ];
    mainProgram = "mujmap";
  };
}
