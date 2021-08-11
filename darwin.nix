{ pkgs, ... }:

let homeDir = builtins.getEnv ("HOME");
in {
  imports = [ <home-manager/nix-darwin> ];

  users.users.michaelwinton = {
    home = homeDir;
    description = "Michael Winton";
    shell = pkgs.zsh;
  };

  environment = {
    shells = with pkgs; [ zsh ];
    loginShell = "/run/current-system/sw/bin/zsh";
  };

  environment.darwinConfig = "${homeDir}/src/github.com/mrwinton/dotfiles/darwin.nix";

  environment.systemPackages = with pkgs; [
    (hunspellWithDicts [
      hunspellDicts.en-gb-ise
      hunspellDicts.en-gb-ize
      hunspellDicts.en-us
      hunspellDicts.sv-se
    ])
    redis
  ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      (nerdfonts.override { fonts = [ "Hack" ]; })
    ];
  };

  home-manager.users.michaelwinton = (import ./home.nix);

  nixpkgs.config.allowUnfree = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  system.defaults = {
    finder = { FXEnableExtensionChangeWarning = false; };
    screencapture = { location = "${homeDir}/Downloads"; };
    trackpad = {
      Clicking = true;
      TrackpadThreeFingerDrag = true;
    };
  };

  system.activationScripts.postActivation.text = ''
    # Use list view in Finder by default
    defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
  '';

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  system.stateVersion = 4;

  nix = {
    buildCores = 4;
    maxJobs = 8;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };
}
