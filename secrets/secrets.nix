let
  # Fetch my keys from github
  githubKeysContent = builtins.fetchurl {
    url = "https://github.com/RobotDisco.keys";
  };
  # Split github content into a list of strings
  # split is weird, gives me empty strings between words
  githubKeys = builtins.filter (x: x != "" && x != [])
    (builtins.split "\n" (builtins.readFile githubKeysContent));
in

{}
