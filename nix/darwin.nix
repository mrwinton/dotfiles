{ pkgs, ... }:
{
  users.users.michaelwinton = {
    description = "Michael Winton";
  };

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
    finder = { FXEnableExtensionChangeWarning = false; };
    screencapture = { location = "~/Downloads"; };
    trackpad = {
      Clicking = true;
      TrackpadThreeFingerDrag = true;
    };
  };

  system.stateVersion = 4;

  nix = {
    buildCores = 4;
    maxJobs = 8;
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
  };
}
