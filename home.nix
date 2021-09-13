{ config, lib, pkgs, ... }:

let
  callPackage = pkgs.callPackage;

  comma = callPackage ./comma/package.nix { };
  hammerspoon = callPackage ./hammerspoon/package.nix { };

  # https://github.com/nix-community/emacs-overlay
  emacs_sha = "4947b85a89f10799ef9b8bc1ee924c47f7919363";

  pkgs_aarch64 = import <nixpkgs> {
    localSystem = "aarch64-darwin";
    overlays = [
      (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${emacs_sha}.tar.gz";
      }))
    ];
  };
in
{
  programs.home-manager.enable = true;

  imports = [
    ./emacs/config.nix
    ./git/config.nix
    ./hammerspoon/config.nix
    ./kitty/config.nix
    ./ruby/config.nix
    ./zsh/config.nix
  ];

  # https://search.nixos.org/packages?channel=unstable
  home.packages = [
    pkgs_aarch64.autoconf
    pkgs_aarch64.automake
    pkgs_aarch64.bat
    pkgs.chromedriver
    pkgs.coreutils-full
    comma
    pkgs_aarch64.emacsGcc
    pkgs_aarch64.fd
    pkgs_aarch64.fzf
    pkgs_aarch64.geckodriver
    pkgs_aarch64.ghq
    pkgs.git
    pkgs.gitAndTools.gh
    pkgs.gitAndTools.hub
    hammerspoon
    pkgs_aarch64.heroku
    pkgs_aarch64.imagemagick
    pkgs_aarch64.nixpkgs-fmt
    pkgs_aarch64.openssl.dev
    pkgs_aarch64.proselint
    pkgs_aarch64.ripgrep
    pkgs_aarch64.stripe-cli
    pkgs_aarch64.tmux
    pkgs_aarch64.youtube-dl
    pkgs_aarch64.universal-ctags
    pkgs.zsh
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
