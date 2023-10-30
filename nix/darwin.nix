{ pkgs, ... }:
{
  users.users.michaelwinton = {
    description = "Michael Winton";
    home = "/Users/michaelwinton";
  };

  environment.systemPackages = with pkgs; [
    redis
  ];

  services.nix-daemon.enable = true;
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
    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
      FXEnableExtensionChangeWarning = false;
      FXPreferredViewStyle = "Nlsv";
    };
    screencapture = { location = "~/Downloads"; };
    trackpad = {
      Clicking = true;
      TrackpadThreeFingerDrag = true;
    };
  };

  system.stateVersion = 4;

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 3d";
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      system = aarch64-darwin
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    settings = {
      cores = 4;
      max-jobs = 8;
    };
  };
}
