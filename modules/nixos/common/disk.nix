{
  # Get emails for any hard drive failures
  services.smartd = {
    enable = true;

    # weekly short tests, monthly long tests
    defaults.autodetected = "-a -o on -s (S/../../3/12|L/../01/./17)";

    notifications = {
      mail = {
        enable = true;

        sender = "root@robot-disco.net";
        recipient = "gdcosta@gmail.com";
      };
    };
  };
}
