{ pkgs, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      pkgs.home-manager
    ];

  nixpkgs.config.allowUnfree = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };

  fonts.packages = [
    pkgs.jetbrains-mono
    pkgs.julia-mono
  ];

  homebrew = {
    enable = true;

    onActivation.autoUpdate = true;
    onActivation.upgrade = true;

    brews = [
      "node"
    ];

    casks = [
      "1password"
      "1password-cli"
      "arc"
      "beekeeper-studio"
      "emacs"
      "firefox"
      "ghostty"
      "google-chrome"
      "hammerspoon"
      "netnewswire"
      "obsidian"
      "whatsapp"
    ];
  };

  system = {
    primaryUser = "michaelwinton";

    defaults = {
      finder = {
        AppleShowAllExtensions = true;
        # Search scope options
        # "This Mac"
        # "currentFolder" 
        # "previousSearch"
        FXDefaultSearchScope = "currentFolder";
        # Preferred view style options
        # "icon"   
        # "list"    
        # "column" 
        # "gallery" 
        FXPreferredViewStyle = "list";
        # Default path for new windows options
        # "Computer"
        # "OS volume"
        # "Home"
        # "Desktop"
        # "Documents"
        # "Recents"
        # "iCloud Drive"
        # "Other"
        NewWindowTarget = "Home";
        FXEnableExtensionChangeWarning = false;
        ShowPathbar = true;
      };
      NSGlobalDomain = {
        # Enable shifting focus in UI elements (e.g. prompts) with TAB
        AppleKeyboardUIMode = 3;
        NSAutomaticWindowAnimationsEnabled = false;
        NSWindowShouldDragOnGesture = true;
      };
      screencapture = { location = "~/Downloads"; };
      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  system.stateVersion = 4;

  nix = {
    # Let Determinate Systems manage Nix instead of nix-darwin
    enable = false;
  };
}
