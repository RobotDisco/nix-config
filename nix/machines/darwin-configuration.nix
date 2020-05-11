{ config, pkgs, ... }:

{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    awscli
    chefdk
    emacs
    fasd
    fd
    fzf
    git
    google-cloud-sdk
    python3
    ripgrep
    tree
    vim
    vscode
    zsh
  ];
  environment.shells = [ pkgs.zsh ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  #programs.bash.enable = true;
  programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 1;
  nix.buildCores = 1;

  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;

  users.users.gaelan = {
    home = "/Users/gaelan";
    description = "Gaelan D'costa";

    shell = pkgs.zsh;
  };

  home-manager.users.gaelan = {config, pkgs, ... }:
    {
      # Let Home Manager install and manage itself.
      programs.home-manager.enable = true;
      
      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      home.stateVersion = "20.03";

      nixpkgs.config.allowUnfree = true;
  
      fonts.fontconfig.enable = true;
      
      home.extraOutputsToInstall = [ "man" "doc" ];

      #programs.chromium.enable = true;
      programs.emacs.enable = true;
      #programs.firefox.enable = true;
      programs.fzf.enable = true;
      programs.fzf.enableZshIntegration = true;
      
      programs.git.enable = true;
      
      programs.git.extraConfig = {
        core = {
          autocrlf = "input";
          editor = "emacsclient";
        };
        hub = {
          protocol = "https";
        };
      };
          
      programs.git.userEmail = "gaelan@tulip.com";
      programs.git.userName = "Gaelan D'costa";

      
      programs.keychain.enable = true;
      programs.keychain.enableZshIntegration = true;
      programs.ssh.enable = true;
      programs.zsh.enable = true;
      programs.zsh.enableAutosuggestions = true;
      programs.zsh.enableCompletion = true;
      programs.zsh.autocd = true;
    };
}
