{ pkgs, ... }:
{
  users.users.michaelwinton = {
    home = "/Users/michaelwinton";
    description = "Michael Winton";
    shell = pkgs.zsh;
  };

  environment = {
    shells = with pkgs; [ zsh ];
    loginShell = "/run/current-system/sw/bin/zsh";
  };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/src/dotfiles/darwin.nix
  environment.darwinConfig = "$HOME/src/dotfiles/darwin.nix";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [
  #   coreutils
  #   openssl
  # ];

  # List fonts installed in system profile
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      powerline-fonts
    ];
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };
  
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  system.defaults = {
    finder = {
      # Show me the file extensions
      AppleShowAllExtensions = true;
      # Don't pop-up when changing extension
      FXEnableExtensionChangeWarning = false;
    };
    screencapture = {
      # Save screenshots to the downloads
      location = "$HOME/Downloads";
    };
    trackpad = {
      # Enable tap-to-click
      Clicking = true;
      # Enable three-finger-drag-to-select
      TrackpadThreeFingerDrag = true;
    };
    NSGlobalDomain = {
      # Show me the file extensions
      AppleShowAllExtensions = true;
      # Show the bars when scrolling
      AppleShowScrollBars = "Automatic";
    };
  };

  system.activationScripts.postActivation.text = ''
    # Use list view in all Finder windows by default
    defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
  '';

  # Avoid backwards incompatible changes.
  system.stateVersion = 4;
}
