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

  # $ darwin-rebuild switch -I darwin-config=$HOME/src/dotfiles/darwin.nix
  environment.darwinConfig = "${homeDir}/src/dotfiles/darwin.nix";

  environment.systemPackages = with pkgs;
    [
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      redis
    ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      powerline-fonts
      (nerdfonts.override { fonts = [ "Meslo" ]; })
    ];
  };

  home-manager.users.michaelwinton = (import ./home.nix);

  nixpkgs.config.allowUnfree = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };

  services.redis = {
    enable = true;
    dataDir = "${homeDir}/.local/share/redis";
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

    # Ensure that redis' data dir exists
    mkdir -p ${homeDir}/.local/share/redis
  '';

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  system.stateVersion = 4;
}
