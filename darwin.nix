{ pkgs, ... }:

let homeDir = builtins.getEnv ("HOME");
in
{
  services.nix-daemon.enable = false;

  users.users.michaelwinton = {
    home = homeDir;
    description = "Michael Winton";
    shell = pkgs.zsh;
  };

  environment = {
    shells = with pkgs; [ zsh ];
    loginShell = "/run/current-system/sw/bin/zsh";
  };

  environment.darwinConfig =
    "${homeDir}/src/github.com/mrwinton/dotfiles/darwin.nix";

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

    # Restart Hammerspoon after the build to apply new configuration
    osascript -e 'quit app "Hammerspoon"'
    open -a Hammerspoon
  '';

  system.stateVersion = 4;

  nix = {
    buildCores = 4;
    maxJobs = 8;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      system = aarch64-darwin
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
  };
}
