{ config, lib, pkgs, ... }:

let
  callPackage = pkgs.callPackage;
  hammerspoon = callPackage ./hammerspoon/package.nix { };

  # https://github.com/nix-community/emacs-overlay
  rev = "5fcafb9229a347b2f5f5dc4ec9f0f5f977b42b85";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
  });
in
{
  programs.home-manager.enable = true;
  nixpkgs.overlays = [ emacs-overlay ];

  imports = [
    ./emacs/config.nix
    ./git/config.nix
    ./hammerspoon/config.nix
    ./ruby/config.nix
    ./zsh/config.nix
  ];

  # https://search.nixos.org/packages?channel=unstable
  home.packages = with pkgs; [
    autoconf
    automake
    bat
    chromedriver
    coreutils-full
    emacsGcc
    fd
    fzf
    geckodriver
    ghq
    git
    hammerspoon
    heroku
    imagemagick
    nixpkgs-fmt
    openssl.dev
    proselint
    ripgrep
    stripe-cli
    tmux
    youtube-dl
    universal-ctags
    zsh
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.activation.copyApplications =
    let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      baseDir="/Applications/Home Manager"
      if [ -d "$baseDir" ]; then
        $DRY_RUN_CMD sudo rm -rf "$baseDir"
      fi
      sudo mkdir -p "$baseDir"
      for appFile in ${apps}/Applications/*; do
        target="$baseDir/$(basename "$appFile")"
        sudo cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
        sudo chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      done
    '';

  home.username = "michaelwinton";
  home.homeDirectory = "/Users/michaelwinton";
  home.stateVersion = "21.05";
}
